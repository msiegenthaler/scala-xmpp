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
 * Base class for the implementation of stateless agents.
 */
trait StatelessAgent extends Agent with ConcurrentObject {
  override def handleIQ(packet: IQRequest) = concurrentWithReply {
    packet match {
      case get: IQGet =>
        val r = handle(iqGet)(get)
        r.getOrElse_cps {
          packet.resultError(StanzaError.badRequest)
        }
      case set: IQSet =>
        val r = handle(iqSet)(set)
        r.getOrElse_cps {
          packet.resultError(StanzaError.badRequest)
        }
    }
  }
  override def handleMessage(packet: MessagePacket) = concurrent {
    handleNoResult(message)(packet)
  }
  override def handlePresence(packet: PresencePacket) = concurrent {
    handleNoResult(presence)(packet)
  }
  override def handleOther(packet: XMPPPacket) = concurrent {
    handleNoResult(other)(packet)
  }

  private def handleNoResult[X](handler: Traversable[Handler[X,_]])(value: X) =
    handler.find(_.isDefinedAt(value)).foreach_cps(_.apply(value))
  private def handle[X,R](handler: Traversable[Handler[X,R]])(value: X) =
    handler.find(_.isDefinedAt(value)).map_cps(_.apply(value))

  protected type Handler[I,O] = PartialFunction[I,O @process]

  protected def iqGet: Seq[Handler[IQGet,IQResponse]] = Nil
  protected def iqSet: Seq[Handler[IQSet,IQResponse]] = Nil
  protected def message: Seq[Handler[MessagePacket,Any]] = Nil
  protected def presence: Seq[Handler[PresencePacket,Any]] = Nil
  protected def other: Seq[Handler[XMPPPacket,Any]] = Nil

  private[component] final def stateless_iqGet = iqGet
  private[component] final def stateless_iqSet = iqSet
  private[component] final def stateless_message = message
  private[component] final def stateless_presence = presence
  private[component] final def stateless_other = other

  protected def mkIqGet(fun: Handler[IQGet,IQResponse]) = fun
  protected def mkIqSet(fun: Handler[IQGet,IQResponse]) = fun
  protected def mkMsg(fun: Handler[MessagePacket,_]) = fun
  protected def mkPres(fun: Handler[PresencePacket,_]) = fun
  protected def mkOther(fun: Handler[XMPPPacket,_]) = fun
}


/**
 * Base class for the implementation of stateful agents (based upon a StateServer).
 */
trait StatefulAgent extends Agent with StateServer {
  override def handleIQ(packet: IQRequest) = call { state =>
    packet match {
      case get: IQGet =>
        val r = handle(iqGet(state))(get, state)
        r.getOrElse_cps {
          (packet.resultError(StanzaError.badRequest), state)
        }
      case set: IQSet =>
        val r = handle(iqSet(state))(set, state)
        r.getOrElse_cps {
          (packet.resultError(StanzaError.badRequest), state)
        }
    }
  }
  override def handleMessage(packet: MessagePacket) = cast { state =>
    handleNoResult(message(state))(packet, state)
  }
  override def handlePresence(packet: PresencePacket) = cast { state =>
    handleNoResult(presence(state))(packet, state)
  }
  override def handleOther(packet: XMPPPacket) = cast { state =>
    handleNoResult(other(state))(packet, state)
  }

  private def handleNoResult[I](handler: Traversable[Handler[I,State]])(value: I, state: State): State @process = {
    val r = handle(handler)(value, state)
    r.getOrElse_cps(state)
  }
  private def handle[I,O](handler: Traversable[Handler[I,O]])(value: I, state: State) = {
    val v = (value, state)
    handler.find(_.isDefinedAt(v)).map_cps(_.apply(v))
  }

  protected val stateless: StatelessAgent = new StatelessAgent{}
  private class StatelessWrapperWithResult[I,O](fun: PartialFunction[I,O @process]) extends Handler[I,(O,State)] {
    override def isDefinedAt(in: (I,State)) = fun.isDefinedAt(in._1)
    override def apply(in: (I,State)) = (fun(in._1), in._2)
  }
  private class StatelessWrapperNoResult[I,O](fun: PartialFunction[I,Any @process]) extends Handler[I,State] {
    override def isDefinedAt(in: (I,State)) = fun.isDefinedAt(in._1)
    override def apply(in: (I,State)) = {
      fun(in._1)
      in._2
    }
  }

  protected type Handler[I,O] = PartialFunction[(I,State),O @process]
  protected def iqGet(state: State): Seq[Handler[IQGet,(IQResponse,State)]] =
    stateless.stateless_iqGet.map(new StatelessWrapperWithResult(_))
  protected def iqSet(state: State): Seq[Handler[IQSet,(IQResponse,State)]] =
    stateless.stateless_iqSet.map(new StatelessWrapperWithResult(_))
  protected def message(state: State): Seq[Handler[MessagePacket,State]] =
    stateless.stateless_message.map(new StatelessWrapperNoResult(_))
  protected def presence(state: State): Seq[Handler[PresencePacket,State]] =
    stateless.stateless_presence.map(new StatelessWrapperNoResult(_))
  protected def other(state: State): Seq[Handler[XMPPPacket,State]] =
    stateless.stateless_other.map(new StatelessWrapperNoResult(_))

  protected def mkIqGet(fun: Handler[IQGet,(IQResponse,State)]) = fun
  protected def mkIqSet(fun: Handler[IQSet,(IQResponse,State)]) = fun
  protected def mkMsg(fun: Handler[MessagePacket,State]) = fun
  protected def mkPres(fun: Handler[PresencePacket,State]) = fun
  protected def mkOther(fun: Handler[XMPPPacket,State]) = fun
}


/**
 * Manages the presence-subscriptions of an agent.
 */
trait PresenceManager extends StatefulAgent with Log {
  protected val services: AgentServices 

  protected override type State <: {
    def friends: Iterable[JID]
  }

  protected override def presence(state: State) = super.presence(state) :+ subscribe :+ unsubscribe :+ probe

  protected def subscribe = mkPres {
    case (Presence(from, content,Some("subscribe"), _, id),state) =>
      log.debug("Subscription request from {}", from)
      val s = acceptSubscription(state)(from, content)
      val response = s.friends.find(_ == from).map { _ =>
        log.info("Subscriber {} added", from)
        Presence(services.jid, NodeSeq.Empty, Some("subscribed"), Some(from), id) //accepted
      }.getOrElse {
        Presence(services.jid, NodeSeq.Empty, Some("unsubscribed"), Some(from), id) //rejected
      }
      services.send(response).receiveOption(1 s)
      s
  }
  protected def unsubscribe = mkPres {
    case (Presence(from,content,Some("unsubscribe"),_,id),state) =>
      log.debug("Unsubscription request from {}", from)
      val s = removeSubscription(state)(from)
      services.send(Presence(services.jid, NodeSeq.Empty, Some("unsubscribed"), Some(from), id))
      s
  }
  protected def probe = mkPres {
    case (Presence(from,content,Some("probe"),_,id),state) =>
      log.debug("Probe from {}", from)
      announce(from)
      state
  }

  protected def announce: Unit @process = asyncCast { state =>
    announce(status(state), state)
  }
  private[this] def announce(status: Status, state: State) = {
    val sends = state.friends.map_cps { friend => 
      services.send(Announce(friend, status.presenceType, status.status))
    }
    sends.foreach_cps(_.receive)
  }
  protected def announce(to: JID) = asyncCast { state =>
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
    asyncCast { s => 
      val status = offlineStatus(s)
      announce(offlineStatus(s), s)
    }
    super.shutdown
  }
      

  /**
   * Check if the subscription is accectable and add it to the state if accepted.
   */
  protected def acceptSubscription(state: State)(from: JID, content: NodeSeq): State @process
  /**
   * Remove the subscription and return the state where it's removed.
   */
  protected def removeSubscription(state: State)(from: JID): State @process

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
    case MessageSend(_, "chat", from, _, content) =>
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
      messageType="chat",
      from=from,
      to=to,
      content=c
    )
  }
}
