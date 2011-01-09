package ch.inventsoft
package scalaxmpp
package component

import scala.xml._
import scala.collection.immutable
import scalabase.process._
import scalabase.oip._
import scalabase.time._
import Messages._


/** Participant of an agent component (JID i.e. agent@component) */
trait Agent {
  /** Name-part of the JID (name@domain/resource) */
  val name: String

  def handleMessage(packet: MessagePacket): Unit @process
  def handleIQ(packet: IQRequest): Selector[IQResponse] @process
  def handlePresence(packet: PresencePacket): Unit @process

  /**
   * The connection to the server has been (re-)established, all methods offered by the
   * callback can now be used.
   * The component will not receive packets (#handle*) until this method completes.
   */
  def connected: Unit @process
  /**
   * The connection to the server has been lost.
   * The manager will try to reestablish the connection and call connected if that succeeds
   */
  def connectionLost: Unit @process
  /** Terminate the agent and release all resources. */
  def shutdown: Unit @process
}
trait AgentCallback {
  /** JID of the agent */
  def jid: JID
  /**
   * Send a generic XMPPPacket without waiting for a response. The Selector waits for the
   * packet to be transmitted to the XMPP-Server
   */
  def send(packet: XMPPPacket): Completion @process
  /**
   * Send an IQ-Request (get/set) and return a selector for the response.
   */
  def request(packet: IQRequest): Selector[IQResponse] @process
  /**
   * Unregister the Agent from the AgentManager. Shutdown will be called.
   */
  def unregister: Unit @process
}


trait AgentManager {
  def register(agent: AgentCallback => Agent @process): Unit @process
}

private[component] trait IQRegister {
  def register(key: IQKey, process: Process): IQRegister
  def remove(key: IQKey): IQRegister
  def remove(process: Process): IQRegister
  def get(key: IQKey): Option[Process]
  def getAndRemove(key: IQKey): Option[(Process,IQRegister)]
}
private[component] class BidiMapIQRegister private(forward: Map[IQKey,Process], reverse: Map[Process,IQKey]) extends IQRegister {
  override def register(key: IQKey, process: Process) = {
    new BidiMapIQRegister(forward + (key -> process), reverse + (process -> key))
  }
  override def remove(key: IQKey) = forward.get(key) match {
    case Some(process) =>
      new BidiMapIQRegister(forward - key, reverse - process)
    case None => this
  }
  override def remove(process: Process) = reverse.get(process) match {
    case Some(key) =>
      new BidiMapIQRegister(forward - key, reverse - process)
    case None => this
  }
  override def get(key: IQKey) = forward.get(key)
  override def getAndRemove(key: IQKey) = forward.get(key) match {
    case Some(process) =>
      val n = new BidiMapIQRegister(forward - key, reverse - process)
      Some((process, n))
    case None => None
  }
}
private[component] object BidiMapIQRegister {
  def apply() = new BidiMapIQRegister(Map(),Map())
}

private[component] class IQKey private(jid: JID, id: String) {
  def matches(packet: IQPacket) = packet match {
    case r: IQResponse =>
      packet.id==id && packet.from==jid
    case r: IQRequest =>
      packet.id==id && packet.to==jid
  }
}
object IQKey {
  def apply(packet: IQPacket) = packet match {
    case packet: IQRequest => new IQKey(packet.to, packet.id)
    case packet: IQResponse => new IQKey(packet.from, packet.id)
  }
}


trait AgentComponent extends XMPPComponent with AgentManager with StateServer {
  protected[this] case class AgentState(agents: Map[JID,AgentHandler], iqRegister: IQRegister, connected: Boolean)
  protected[this] override type State = AgentState

  protected[this] val componentJID: JID
  protected[this] val manager: XMPPComponentManager

  protected[this] override def init = {
    AgentState(Map() + (SelfAgent.jid -> SelfAgent), BidiMapIQRegister(), false)
  }
  protected[this] override def handler(state: State) = super.handler(state).orElse_cps {
    case ProcessCrash(process, _) =>
      val niqr = state.iqRegister.remove(process)
      Some(state.copy(iqRegister=niqr))
    case ProcessKill(process, _, _) =>
      val niqr = state.iqRegister.remove(process)
      Some(state.copy(iqRegister=niqr))
  }

  def register(creator: AgentCallback => Agent @process) = cast { state =>
    val handler = AgentHandler(creator)
    if (state.connected) spawnChild(Required) { handler.agent.connected }
    state.copy(agents=state.agents + (handler.jid -> handler))
  }
  protected[this] def unregister(jid: JID) = cast { state =>
    state.agents.get(jid).foreach_cps { agent =>
      spawnChild(Required)(agent.shutdown)
    }
    state.copy(agents=state.agents - jid)
  }

  override def connected = cast { state =>
    foreachAgent(state, _.connected)
    state.copy(connected=true)
  }
  override def connectionLost = cast { state =>
    foreachAgent(state, _.connectionLost)
    state.copy(connected=false)
  }
  override def shutdown = cast { state =>
    foreachAgent(state, _.shutdown)
    stop
    state.copy(agents = Map())
  }
  protected[this] def foreachAgent(state: State, fun: Agent => Unit @process) = {
    state.agents.values.foreach_cps(agent => spawnChild(Required)(fun(agent.agent)))
  }

  protected[this] val iqTimeout = 10 minutes

  override def process(packet: XMPPPacket) = asyncCast { state => packet match {
    case m: IQRequest =>
      state.agents.get(m.to) match {
        case Some(agent) =>
          val response = agent.agent.handleIQ(m).receiveOption(iqTimeout)
          response match {
            case Some(response) => manager.send(response)
            case None => manager.send(m.resultError(StanzaError.remoteServerTimeout))
          }
        case None =>
          m.resultError(StanzaError.itemNotFound).copy(from=componentJID)
      }
    case m: IQResponse =>
      val key = IQKey(m)
      state.iqRegister.getAndRemove(key) match {
        case Some((process, niqr)) =>
          process ! m
          state.copy(iqRegister=niqr)
        case None =>
          //ignore response to unknown request
          state
      }

    case m: PresencePacket => m.toOption match {
      case Some(to) => state.agents.get(to) match {
        case Some(agent) =>
          agent.agent.handlePresence(m)
        case None => if (!m.isError) {
          val error = PresenceError(componentJID, StanzaError.itemNotFound, m.idOption, Some(m.from))
          manager.send(error)
        }
      }
      case None =>
        state.agents.values.foreach_cps(_.agent.handlePresence(m))
    }

    case m: MessagePacket =>
      state.agents.get(m.to) match {
        case Some(agent) =>
          agent.agent.handleMessage(m)
        case None => if (!m.isError) {
          val error = MessageError(m.id, componentJID, m.from, StanzaError.itemNotFound, m.content)
          manager.send(error)
        }
      }

    case m: XMPPPacket =>
      //Ignore other packets
  }}

  protected[this] def handleMessage(packet: MessagePacket) = packet match {
    case MessageSend(_, "chat", from, _, content) =>
      content.find(_.label == "body").map(_.text.toLowerCase) match {
        case Some("list") => asyncCast { state =>
          log.debug("Listing requested by {}", from)
          val msg = state.agents.map(_._1).filter(_ != componentJID).map(_.stringRepresentation).mkString("<br/>")
          sendChatMessage(from, msg)
        }
        case Some(text) =>
          log.debug("Message '{}' received from {}", text, from)
          sendChatMessage(from, "I don't understand '"+text+"'\nKnown commands:\n list")
        case None => //don't react to messages without a body
      }
    case other => //don't react to all other messages
  }
  protected[this] def sendChatMessage(to: JID, body: String) = {
    val c = <subject>Answer</subject><body>{body}</body>;
    manager.send(MessageSend(
      id=None,
      messageType="chat",
      from=componentJID,
      to=to,
      content=c
    ))
    ()
  }

  protected[this] def handleIQ(packet: IQRequest) = async { state => 
    val response = packet match {
      case get @ IQGet(_, from, _, content) => content.headOption match {
        case Some(e @ <query/>) if e.namespace=="http://jabber.org/protocol/disco#info" =>
          log.debug("Discovery request from {}", from)
          get.resultOk {
            <query xmlns="http://jabber.org/protocol/disco#items">
            {state.agents.map(_._1).map { agent =>
              <item jid="{agent.stringRepresentation}"/>
            }}
            </query>
          }
        case Some(other) =>
          log.debug("Unsupported IQ-Get from {} received: {}:{}", from, other.label, other.namespace)
          get.resultError(StanzaError.badRequest)
        case None =>
          log.debug("Malformed IQ-Get from {} received (no content)", from)
          get.resultError(StanzaError.badRequest)
      }
      case set @ IQSet(_, from, _, _) =>
        log.debug("Received IQ-Set from {}", from)
        set.resultError(StanzaError.badRequest)
    }
    response
  }

  protected[this] def handlePresence(packet: PresencePacket) = packet match {
    case Presence(from, content, pt, to, id) =>
      //ignore presence messages
    case PresenceError(from, error, id, to, content) =>
      //ignore presence errors
  }

  protected[this] object SelfAgent extends AgentHandler {
    override val jid = componentJID
    override val agent = new Agent {
      override val name = "self"
      override def handleMessage(packet: MessagePacket) = AgentComponent.this.handleMessage(packet)
      override def handleIQ(packet: IQRequest) = AgentComponent.this.handleIQ(packet)
      override def handlePresence(packet: PresencePacket) = AgentComponent.this.handlePresence(packet)
      override def connected = log.debug("Connected to XMPPServer")
      override def connectionLost = log.debug("Connection to XMPPServer lost")
      override def shutdown = noop
    }
    override def unregister = ()
  }

  protected[this] def registerIQ(key: IQKey, process: Process) = cast { state =>
    state.copy(iqRegister=state.iqRegister.register(key, process))
  }

  protected[this] trait AgentHandler extends AgentCallback {
    def agent: Agent
    def jid: JID
    override def send(packet: XMPPPacket) = manager.send(packet)
    override def request(packet: IQRequest) = {
      val key = IQKey(packet)
      registerIQ(key, self)
      manager.send(packet)
      new MessageSelector[IQResponse] {
        override def isDefinedAt(msg: Any) = msg match {
          case answer: IQResponse => key.matches(answer)
          case other => false
        }
        override def apply(msg: Any) = msg.asInstanceOf[IQResponse]
      }
    }
    override def unregister = AgentComponent.this.unregister(jid)
    def shutdown = agent.shutdown
  }
  protected[this] object AgentHandler {
    def apply(creator: AgentCallback => Agent @process): AgentHandler @process = {
      new AgentHandler {
        var agent: Agent = null
        var jid: JID = null
        def create = {
          agent = creator(this)
          jid = JID(agent.name, componentJID.domain)
          this
        }
      }.create
    }
  }
}
object AgentComponent {
  def specification(name: String, description: String, subdomain: String, secret: Option[String]=None)(init: AgentManager => Unit @process = agent => noop) = {
    val n = name; val d = description; val s = subdomain; val sc = secret;
    new XMPPComponentSpecification() with ConcurrentObject {
      override val name = n
      override val description = d
      override val subdomain = s
      override val secret = sc
      override def initializeComponent(jid: JID, mgr: XMPPComponentManager) = replyInCallerProcess {
        val spec = this
        val component = new AgentComponent {
          override val manager = mgr
          override val specification = spec
          override val componentJID = jid
        }
        Spawner.start(component, SpawnAsRequiredChild)
        init(component)
        component
      }
    }
  }
}
