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
trait Agent extends Spawnable {
  def handleIQ(packet: IQRequest): Selector[IQResponse] @process = {
    val r = RequestToken[IQResponse]
    r.reply(packet.resultError(StanzaError.badRequest))
    r.select
  }
  def handleMessage(packet: MessagePacket): Unit @process = noop
  def handlePresence(packet: PresencePacket): Unit @process = noop
  def handleOther(packet: XMPPPacket): Unit @process = noop

  /**
   * The connection to the server has been (re-)established, all methods offered by the
   * callback can now be used.
   * The component will not receive packets (#handle*) until this method completes.
   */
  def connected: Unit @process = noop
  /**
   * The connection to the server has been lost.
   * The manager will try to reestablish the connection and call connected if that succeeds
   */
  def connectionLost: Unit @process = noop
  /** Terminate the agent and release all resources. */
  def shutdown: Unit @process = noop

  private[component] def agentProcess = process
  
  override def toString = "Agent"
}

/** Functions available to an agent */
trait AgentServices {
  /** JID of the agent */
  val jid: JID
  /** JID of the responsible server */
  val serverJid: JID

  /**
   * Send a generic XMPPPacket without waiting for a response. The Selector waits for the
   * packet to be transmitted to the XMPP-Server
   */
  def send(packet: XMPPPacket): Completion @process

  /**
   * Send an IQ-request (get/set) and return the selector for the response.
   */
  def request(packet: IQRequest): Selector[IQResponse] @process
  /** Generate a new, unique id for an IQ-request. */
  def iqId: String
  /** Send an IQGet and return the selector for the response. to=None => server */
  def iqGet(to: Option[JID]=None, content: NodeSeq): Selector[IQResponse] @process = {
    request(IQGet(iqId, jid, to.getOrElse(serverJid), content))
  }
  /** Send an IQSet and return the selector for the response. to=None => server */
  def iqSet(to: Option[JID]=None, content: NodeSeq): Selector[IQResponse] @process = {
    request(IQSet(iqId, jid, to.getOrElse(serverJid), content))
  }

  /**
   * Unregister the Agent from the AgentManager. Shutdown will be called.
   */
  def unregister: Completion @process
}

trait AgentManager {
  /**
   * Adds a new agent.
   * @name Name-part of the JID (name@domain)
   */
  def register(name: String, agent: AgentServices => Agent): Unit @process

  /** View of all currently registered agents */
  def registeredComponents: Selector[Map[JID,Agent]] @process
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
  override def toString = forward.toString
}
private[component] object BidiMapIQRegister {
  def apply() = new BidiMapIQRegister(Map(),Map())
}

private[component] class IQKey private(val jid: JID, val id: String) {
  def matches(packet: IQPacket) = packet match {
    case r: IQResponse =>
      packet.id==id && packet.from==jid
    case r: IQRequest =>
      packet.id==id && packet.toOption==Some(jid)
  }
  override def equals(o: Any) = o match {
    case other: IQKey => jid==other.jid && id==other.id
  }
  override def hashCode = jid.hashCode ^ id.hashCode
  override def toString = jid.toString + ":" + id
}
object IQKey {
  def apply(packet: IQPacket) = packet match {
    case packet: IQRequest => new IQKey(packet.to, packet.id)
    case packet: IQResponse => new IQKey(packet.from, packet.id)
  }
}


trait AgentComponent extends XMPPComponent with AgentManager with StateServer {
  protected case class State(agents: Map[JID,AgentHandler], iqRegister: IQRegister, connected: Boolean)

  protected val componentJID: JID
  protected val serverJID: JID
  protected val manager: XMPPComponentManager

  protected override def init = {
    State(Map() + (DomainHandler.jid -> DomainHandler), BidiMapIQRegister(), false)
  }
  protected override def handler(state: State) = {
    def handleCrash(process: Process) = {
      val niqr = state.iqRegister.remove(process)
      val agent = agentForProcess(process)(state)
      agent.foreach_cps(h => register(h.name, h.creator))
      Some(state.copy(iqRegister=niqr))
    }
    super.handler(state).orElse_cps {
      case ProcessCrash(process, _) => handleCrash(process)
      case ProcessKill(process, _, _) => handleCrash(process)
    }
  }
  private def agentForProcess(process: Process)(state: State) =
    state.agents.map(_._2).find(_.process == process)


  protected val removeTimeout = 1 minute
  override def register(name: String, creator: AgentServices => Agent) = concurrent { state =>
    val handler = AgentHandler(name, creator)
    unregister(handler.jid).receiveOption(removeTimeout)
    handler.start
    atomic { state =>                                                                                              
      if (state.connected) spawnChild(Required) { handler.agent.connected }
      state.copy(agents=state.agents + (handler.jid -> handler))
    }
  }
  protected def unregister(jid: JID) = async { state =>
    atomic(state => state.copy(agents=state.agents-jid))
    state.agents.get(jid).foreach_cps(_.shutdown)    
  }

  override def registeredComponents = get(_.agents.mapValues(_.agent))

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
  protected def foreachAgent(state: State, fun: Agent => Unit @process) = {
    state.agents.values.foreach_cps(agent => spawnChild(Required)(fun(agent.agent)))
  }

  protected val iqTimeout = 10 minutes

  override def process(packet: XMPPPacket) = concurrent { state => packet match {
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
          log.debug("Response to unknown IQ-request: {}. Will be ignored", key)
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
      //see if the packet has a parsable to attribute
      m.xml.attribute("to").map(_.toString).flatMap(JID.parseOption _).flatMap(state.agents.get _) match {
        case Some(agent) =>
          agent.agent.handleOther(m)
        case None =>
          //ignore
      }
  }}

  protected def registerIQ(key: IQKey, process: Process) = cast { state =>
    state.copy(iqRegister=state.iqRegister.register(key, process))
  }
  
  override def toString = "AgentComponent("+componentJID+")"

  protected trait AgentHandler extends AgentServices {
    val name: String
    val creator: AgentServices => Agent

    def agent: Agent
    def process = agent.agentProcess
    def start = Spawner.start(agent, SpawnAsMonitoredChild)
    override val serverJid = serverJID
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
    private[this] val idDealer = new java.util.concurrent.atomic.AtomicLong
    override def iqId = idDealer.incrementAndGet.toString
    override def unregister = AgentComponent.this.unregister(jid)
    def shutdown = agent.shutdown
  }
  protected object AgentHandler {
    def apply(name: String, creator: AgentServices => Agent): AgentHandler @process = {
      val c = creator
      val n = name
      new AgentHandler {
        override val creator = c
        override val name = n
        var agent: Agent = null
        val jid: JID = JID(name, componentJID.domain)
        def create = {
          agent = creator(this)
          this
        }
      }.create
    }
  }

  protected object DomainHandler extends AgentHandler {
    val creator = createDomainAgent _
    val name = null 
    val jid = componentJID
    val agent = createDomainAgent(this)
    protected def createDomainAgent(services: AgentServices): Agent = {
      new ComponentDiscoveryAgent {
        override val services = DomainHandler.this
        override type State = Unit
        override def init = ()
          override val manager = AgentComponent.this
      }
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
      override def initializeComponent(jid: JID, serverJid: JID, mgr: XMPPComponentManager) = replyInCallerProcess {
        val spec = this
        val component = new AgentComponent {
          override val manager = mgr
          override val specification = spec
          override val componentJID = jid
          override val serverJID = serverJid
        }
        Spawner.start(component, SpawnAsRequiredChild)
        init(component)
        component
      }
    }
  }
}
