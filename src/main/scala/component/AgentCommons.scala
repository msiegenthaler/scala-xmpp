package ch.inventsoft
package scalaxmpp
package component

import scala.xml._
import scalabase.process._
import scalabase.time._
import scalabase.oip._
import scalabase.log._
import Messages._

/**
 * A agent that has a state
 */
trait StatefulAgent { self: Log =>
  protected type State
  protected val services: AgentServices
  protected def handleIQ(packet: IQRequest, state: State): IQResponse @process = {
    log.debug("Unhandled IQ received from {}", packet.from)
    packet.resultError(StanzaError.badRequest)
  }
  protected def handleMessage(packet: MessagePacket, state: State): Unit @process = {
    log.debug("Unhandled Message received from {}", packet.from)
    val msg = MessageError(packet.id, services.jid, packet.from, StanzaError.badRequest, packet.content)
    services.send(msg).receive
  }
  protected def handlePresence(packet: PresencePacket, state: State): Unit @process = {
    log.debug("Unhandled Presence received from {}", packet.from)
    val msg = PresenceError(services.jid, StanzaError.badRequest, packet.idOption, Some(packet.from), packet.content)
    services.send(msg).receive
  }
  protected def handleOther(packet: XMPPPacket, state: State): Unit @process = {
    log.debug("Unhandled XMPPPacket received")
  }
}

/**
 * Base class for the implementation of stateful agents (based upon a StateServer).
 */
abstract class StateServerAgent extends StateServer with Agent { self: StatefulAgent =>
  override final def handleIQ(packet: IQRequest) =
    async(self.handleIQ(packet, _))
  override final def handleMessage(packet: MessagePacket) =
    concurrent(self.handleMessage(packet, _))
  override final def handlePresence(packet: PresencePacket) =
    concurrent(self.handlePresence(packet, _))
  override final def handleOther(packet: XMPPPacket) =
    concurrent(self.handleOther(packet, _))

  override def toString = "StateServerAgent"
}


/**
 * Trait for splitting the handleMethods into multiple PartialFunction, each handling a specific request.
 */
trait HandlerAgent extends StatefulAgent { self: Log =>
  protected override def handleIQ(packet: IQRequest, state: State) = {
    val r = packet match {
      case get: IQGet => handle(_iqGet)(get, state)
      case set: IQSet => handle(_iqSet)(set, state)
      case _ => None
    }
    r.getOrElse_cps(super.handleIQ(packet, state))
  }
  protected override def handleMessage(packet: MessagePacket, state: State) = {
    val r = handleNoResult(_message)(packet, state)
    r.getOrElse_cps(super.handleMessage(packet, state))
  }
  protected override def handlePresence(packet: PresencePacket, state: State) = {
    val r = handleNoResult(_presence)(packet, state)
    r.getOrElse_cps(super.handlePresence(packet, state))
  }
  protected override def handleOther(packet: XMPPPacket, state: State) = {
    val r = handleNoResult(_other)(packet, state)
    r.getOrElse_cps(super.handleOther(packet, state))
  }

  private def handleNoResult[I](handler: Traversable[Handler[I,Unit]])(value: I, state: State) = {
    handle(handler)(value, state)
  }
  private def handle[I,O](handler: Traversable[Handler[I,O]])(value: I, state: State): Option[O] @process = {
    val v = (value, state)
    handler.find(_.isDefinedAt(v)).map_cps(_.apply(v))
  }

  protected type Handler[I,O] = PartialFunction[(I,State),O @process]

  /** Handlers for IQ-Get requests. Will be cached */
  protected def iqGet: Seq[Handler[IQGet,IQResponse]] = Nil
  /** Handlers for IQ-Set requests. Will be cached */
  protected def iqSet: Seq[Handler[IQSet,IQResponse]] = Nil
  /** Handlers for Messages. Will be cached */
  protected def message: Seq[Handler[MessagePacket,Unit]] = Nil
  /** Handlers for Presence-Messages. Will be cached */
  protected def presence: Seq[Handler[PresencePacket,Unit]] = Nil
  /** Handler for other XMPP-Messages. Will be cached */
  protected def other: Seq[Handler[XMPPPacket,Unit]] = Nil

  private[this] lazy val _iqGet = iqGet
  private[this] lazy val _iqSet = iqSet
  private[this] lazy val _message = message
  private[this] lazy val _presence = presence
  private[this] lazy val _other = other

  protected def mkIqGet(fun: Handler[IQGet,(IQResponse)]) = fun
  protected def mkIqSet(fun: Handler[IQSet,(IQResponse)]) = fun
  protected def mkMsg(fun: Handler[MessagePacket,Unit]) = fun
  protected def mkPres(fun: Handler[PresencePacket,Unit]) = fun
  protected def mkOther(fun: Handler[XMPPPacket,Unit]) = fun
  
  override def toString = "HandlerAgent"
}

/**
 * Agent that does only allows subscription requests from unknown JIDs, all other commands are blocked and
 * answered with 'subscriptionRequired'.
 */
trait ProtectedAgent extends StatefulAgent { self: Log =>
  protected val services: AgentServices

  protected def notAFriendError = StanzaError.subscriptionRequired
  protected override def handleIQ(packet: IQRequest, state: State) = {
    if (isFriend(packet.from)(state))
      super.handleIQ(packet, state)
    else {
      log.debug("Rejected IQ from {} (not a friend)", packet.from)
      packet.resultError(notAFriendError)
    }
  }
  protected override def handleMessage(packet: MessagePacket, state: State) = {
    if (isFriend(packet.from)(state)) 
      super.handleMessage(packet, state)
    else {
      log.debug("Rejected Message from {} (not a friend)", packet.from)      
      services.send(MessageError(packet.id, services.jid, packet.from, notAFriendError, packet.content)).receive
    }
  }
  protected override def handlePresence(packet: PresencePacket, state: State) = {
    if (isFriend(packet.from)(state))
      super.handlePresence(packet, state)
    else packet match {
      case Presence(_, _, Some("subscribe"), _, _) =>
        super.handlePresence(packet, state)
      case _ =>
        log.debug("Rejected Presence from {} (not a friend)", packet.from)
        val msg = PresenceError(services.jid, notAFriendError, packet.idOption, Some(packet.from), packet.content)
        services.send(msg).receive
    }
  }
  protected override def handleOther(packet: XMPPPacket, state: State) = {
    //deny all since we have no way to parse the from-JID
    log.info("Ignored other xmpp-packet, no way to authorize")
  }

  protected def isFriend(jid: JID)(state: State): Boolean
}

/**
 * Manages the presence-subscriptions of an agent.
 */
trait PresenceManager extends Agent with HandlerAgent { self: Log =>
  protected val services: AgentServices 

  protected override type State <: {
    def friends: Iterable[JID]
  }

  protected override def presence = super.presence :+ subscribe :+ unsubscribe :+ probe

  protected def atomic(fun: State => State @process): State @process
  protected def concurrent(fun: State => Any @process): Unit @process

  protected val subscribe = mkPres {
    case (Presence(from, content,Some("subscribe"), _, id),state) =>
      log.debug("Subscription request from {}", from)
      val s = atomic(acceptSubscription(from, content) _)
      val response = s.friends.find(_ == from).map { _ =>
        log.info("Subscriber {} added", from)
        Presence(services.jid, NodeSeq.Empty, Some("subscribed"), Some(from), id) //accepted
      }.getOrElse {
        Presence(services.jid, NodeSeq.Empty, Some("unsubscribed"), Some(from), id) //rejected
      }
      services.send(response).receive
  }
  protected val unsubscribe = mkPres {
    case (Presence(from,content,Some("unsubscribe"),_,id),state) =>
      log.debug("Unsubscription request from {}", from)
      atomic(removeSubscription(from) _)
      services.send(Presence(services.jid, NodeSeq.Empty, Some("unsubscribed"), Some(from), id)).receive
  }
  protected val probe = mkPres {
    case (Presence(from,content,Some("probe"),_,id),state) =>
      log.debug("Probe from {}", from)
      announce(from)
  }

  protected def isFriend(jid: JID)(state: State) = {
    state.friends.find(_.isParentOf(jid)).isDefined
  }

  protected def announce: Unit @process = concurrent { state =>
    announce(status(state), state)
  }
  private def announce(status: Status, state: State) = {
    val sends = state.friends.map_cps { friend => 
      services.send(Announce(friend, status.presenceType, status.status))
    }
    sends.foreach_cps(_.receive)
  }
  protected def announce(to: JID) = concurrent { state =>
    val stat = status(state)
    services.send(Announce(to, stat.presenceType, stat.status)).receive
  }
  private object Announce {
    def apply(to: JID, presenceType: Option[String], content: NodeSeq) =
      Presence(from=services.jid, to=Some(to), content=content, presenceType=presenceType)
  }

  override def connected = {
    super.connected
    announce
  }
  override def shutdown = {
    concurrent { s => 
      val status = offlineStatus(s)
      announce(offlineStatus(s), s)
    }
    super.shutdown
  }
      

  /**
   * Check if the subscription is accectable and add it to the state if accepted.
   */
  protected def acceptSubscription(from: JID, content: NodeSeq)(state: State): State @process
  /**
   * Remove the subscription and return the state where it's removed.
   */
  protected def removeSubscription(from: JID)(state: State): State @process

  protected case class Status(status: NodeSeq, presenceType: Option[String] = None)
  /**
   * Return the current status of the Agent and an optional presence type.
   * i.e. None,<show>chat</show><status>Ready to rock the world</status>
   */
  protected def status(state: State) = {
    val s = <show>chat</show><status>Active</status>;
    Status(s)
  }
  /**
   * Status for this agen when it's offline
   */
  protected def offlineStatus(state: State) = {
    val s = <status>Offline</status>;
    Status(s, Some("unavailable"))
  }
}


//Helpers for parsing


object FirstElem {
  /** The first xml-element inside a xmpp packet */
  def unapply(packet: XMPPPacket) = firstElem(packet.xml.child)

  def firstElem(ns: NodeSeq): Option[Elem] = 
    subElems(ns).headOption
  def subElems(ns: NodeSeq): Seq[Elem] = 
    ns.map(onlyElems _).flatten
  def onlyElems(node: Node): Option[Elem] = node match {
    case e: Elem => Some(e)
    case _ => None
  }
}
object ElemName {
  /** Name and namespace of an Elem */
  def unapply(elem: Elem) = Some(elem.label, elem.namespace)
}


/** XMPP Chat Messages */
object Chat {
  /** (Subject, Thread, Body, Sender) */
  def unapply(msg: MessagePacket) = msg match {
    case MessageSend(_, Some("chat"), from, _, content) =>
      content.find(_.label=="body").map { body =>
        val subject = content.find(_.label=="subject")
        val thread = content.find(_.label=="thread").map(_.text)
        (subject, thread, body.text, from)
      }
    case _ => None
  }
  def apply(subject: Option[NodeSeq], thread: Option[String], body: String, to: JID, from: JID) = {
    val parts = (
      subject.map(s => <subject>{s}</subject>) ::
      thread.map(t => <thread>{t}</thread>) :: 
      Some(body).map(b => <body>{b}</body>) :: Nil
    ).flatten
    val c = parts.foldLeft(NodeSeq.Empty)(_ ++ _)
    MessageSend(
      id=None,
      messageType=Some("chat"),
      from=from,
      to=to,
      content=c
    )
  }
}
