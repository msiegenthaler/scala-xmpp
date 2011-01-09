package ch.inventsoft
package scalaxmpp
package component

import scala.xml._
import java.nio.charset.Charset
import scalabase.process._
import Messages._
import scalabase.oip._
import scalabase.io._
import scalabase.time._


/**
 * Specification an XMPPComponent can be created from.
 */
trait XMPPComponentSpecification {
  val name: String
  val description: String
  /** subdomain to be used for the component */
  val subdomain: String
  /** Secret used to autenticate with the XMPP-server */
  def secret: Option[String]

  /** Create the XMPPComponent for this specification. Is called by XMPPComponentServer#add. */
  protected[component] def initializeComponent(
    jid: JID,
    serverJid: JID,
    manager: XMPPComponentManager): Selector[XMPPComponent] @process
}

/**
 * XMPP component as per XEP-0114. Components enhance the functionality of an XMPP server.
 * The all methods must be implemented as very short running and non-blocking. Proposed implementation
 * is based upon processes (either a StateServer or by spawning a process per method invocation).
 */
trait XMPPComponent {
  /** specification this component was created from */
  val specification: XMPPComponentSpecification

  /**
   * The connection to the server has been (re-)established, all methods offered by the
   * manager can now be used.
   * The component will not receive packets (#process(XMPPPacket)) until this method
   * completes.
   */
  def connected: Unit @process

  /** Process a packet that was delivered to the component. */
  def process(packet: XMPPPacket): Unit @process

  /**
   * The connection to the server has been lost.
   * The manager will try to reestablish the connection and call connected if that succeeds
   */
  def connectionLost: Unit @process

  /** Terminate the component and release all resources. */
  def shutdown: Unit @process
}


/**
 * Manager for a (single) XMPP component.
 */
trait XMPPComponentManager {
  /**
   * Send an XMPP packet.
   * Only use when connected, else packets will be ignored.
   */
  def send(packet: XMPPPacket): Completion @process

  /**
   * Removes the component.
   * Shutdown will be called (but not connection lost)
   */
  def unregister: Unit @process
}


/**
 * A (remote)  XMPP-Server.
 */
trait XMPPComponentServer {
  /**
   * Register a new XMPP-Component for the server.
   */
  def register(componentSpec: XMPPComponentSpecification): Completion @process
}



object XMPPComponentServer {
  def tcp(host: String, port: Int, defaultSecret: Option[String]=None): XMPPComponentServer @process = {
    val connectionManager = tcpConnectionManager(host, port, defaultSecret)
    new XMPPComponentServer with ConcurrentObject {
      override def register(componentSpec: XMPPComponentSpecification) = concurrentWithReply {
        val wrapper = new ComponentWrapper(componentSpec, connectionManager, keepAliveInterval)
        Spawner.start(wrapper, SpawnAsRequiredChild)
        ()
      }
      override def toString = "<XMPPServer "+host+":"+port+">"
    }
  }

  protected val keepAliveInterval = 1 minute

  private def tcpConnectionManager(hostname: String, portnumber: Int, defaultSecret: Option[String]) = {
    val cf = new SocketXMPPConnectionFactory {
      override val host = hostname
      override val port = portnumber
    }
    val ds = defaultSecret
    new BaseConnectionManager {
      override def connectionFactory = cf
      override val defaultSecret = ds
    }
  }
}


protected[component] trait ConnectionManager {
  def open(subdomain: String, secret: Option[String]): Connection @process
}
protected[component] trait Connection {
  val port: CommunicationPort[XMPPPacket,XMPPPacket]
  val streamId: String
  val from: JID
  def close = port.close
}

protected[component] trait BaseConnectionManager extends ConnectionManager {
  protected val encoding = Charset.forName("UTF-8")
  protected[this] val connectionEstablishTimeout = 5 s
  protected[this] val connectionAnswerTimeout = 2 s

  protected[this] def connectionFactory: XMPPConnectionFactory
  protected[this] def defaultSecret: Option[String]

  override def open(subdomain: String, secret: Option[String]) = {
    val root = <stream:stream xmlns="jabber:component:accept" 
                              xmlns:stream="http://etherx.jabber.org/streams"
                              to={subdomain}/>
    val connectionPort = connectionFactory.openConnection(root).receiveOption(connectionEstablishTimeout)
    connectionPort match {
      case Some(connectionPort) =>
        val stream = receiveStreamStart(connectionPort)
        val id = (stream \ "@id").text
        if (id == null || id.isEmpty) throw new XMPPException("Did not receive a stream-id from server")
        val conn = new Connection {
          override val port = connectionPort
          override val streamId = id
          override val from = JID.parse((stream \ "@from").text)
          override def toString = "<XMPPConnection for "+subdomain+">"
        }
        val sec = secret.getOrElse(defaultSecret.getOrElse(throw new XMPPException("No secret specified")))
        performHandshake(conn, sec)
        conn
      case None => noop; throw new XMPPException("Could not open connection, no answer received")
    }
  }

  private def receiveStreamStart(connection: Source[XMPPPacket]) = {
    val read = connection.readWithin(connectionAnswerTimeout, 1)
    read match {
      case Some(Data(packets)) => packets.head match {
        case OtherXMPPPacket(data) if data.namespace=="http://etherx.jabber.org/streams" && data.label=="stream" =>
          data
        case other =>
          throw XMPPException("Unexpected element "+other.xml)
      }
      case Some(EndOfData) =>
        throw XMPPException("Server closed the connection without sending a <stream>-tag")
      case None =>
        throw new XMPPException("No answer from server (expected a <stream>)")
    }
  }
  private def performHandshake(connection: Connection, secret: String) = {
    val handshake = XMPPComponentHandshakeClient(secret, connection.streamId, encoding)
    val port = connection.port
    port.write(handshake).await(connectionAnswerTimeout)
    val response = port.readWithin(connectionAnswerTimeout, 1).
          getOrElse(throw new XMPPException("No answer received from server (handshake)"))
    val data = response match {
      case Data(data) => data.head
      case EndOfData => throw new XMPPException("Server closed the connection during handshalke")
    }
    data match {
      case p @ OtherXMPPPacket(XMPPComponentHandshakeServer.xml) =>
        //ok, server accepted the secret
        ()
      case error: StreamError =>
        throw new StreamException(error.content)
      case other =>
        throw new XMPPException("Received unexpected handshake answer: "+other)
    }
  }
}

protected[component] class ComponentWrapper(
    val spec: XMPPComponentSpecification,
    connectionMgr : ConnectionManager,
    keepAliveInterval: Duration) extends StateServer {

  protected[this] override type State = ComponentWrapperState
  protected[this] case class ComponentWrapperState(
    component: Option[XMPPComponent],
    connector: Option[Process],
    reader: Option[Process],
    keepalive: Process,
    connection: Option[Connection])

  protected[this] override def init = {
    val keepalive = spawnChild(Required) {
      //keep-alive
      def run: Unit @process = receiveWithin(keepAliveInterval) {
        case Terminate => ()
        case Timeout => sendKeepAlive; run
      }
      run
    }
    openConnection
    ComponentWrapperState(None, None, None, keepalive, None)
  }
  protected[this] override def handler(state: State) = {
    type HFun = PartialFunction[Any,Option[State] @process]
    def wait(sleep: Duration) = receiveWithin(sleep) {
      case Timeout => ()
    }
    val reconnectWait = 5 s
    def connectorWatcher(chain: HFun): HFun = state.connector match {
      case Some(connector) => chain.orElse_cps {
        case ProcessExit(`connector`) =>
          //normal exit, must have been triggered by us, so don't notify or reestablish
          Some(state.copy(connector=None))
        case ProcessCrash(`connector`, reason) =>
          log.info("Could not connect to the XMPP-Server, waiting for {} until trying again", reconnectWait)
          wait(reconnectWait)
          connectionLost(state)
          Some(state.copy(connection=None, connector=None))
        case ProcessKill(`connector`, _, reason) =>
          log.info("Could not connect to the XMPP-Server, waiting for {} until trying again", reconnectWait)
          wait(reconnectWait)
          connectionLost(state)
          Some(state.copy(connection=None, connector=None))
      }
      case None => chain
    }
    def readerWatcher(chain: HFun): HFun = state.reader match {
      case Some(reader) => chain.orElse_cps {
        case ProcessExit(`reader`) => Some(state.copy(reader=None))
        case ProcessCrash(`reader`, reason) => //reader crashed, restart
          log.warn("XMPP-Reader for {} crashed with, restarting", spec.name, reason)
          Some(startReader(state))
        case ProcessKill(`reader`, _, reason) => //reader crashed, restart
          log.warn("XMPP-Reader for {} crashed with, restarting", spec.name, reason)
          Some(startReader(state))
      }
      case None => chain
    }
    connectorWatcher(super.handler(state))
  }
  protected[this] override def termination(state: State) = {
    state.keepalive ! Terminate
    state.reader.foreach_cps(_ ! Terminate)
    state.connector.foreach_cps(_ ! Terminate)
  }

  protected[this] def openConnection = cast { state =>
    state.connector.foreach_cps(_ ! Terminate)
    state.reader.foreach_cps(_ ! Terminate)
    val connector = spawnChild(Monitored) {
      val connection = connectionMgr.open(spec.subdomain, spec.secret)
      connected(connection)
    }
    state.copy(connector=Some(connector))
  }
  protected[this] def connected(connection: Connection) = cast { state =>
    log.debug("Established connection to XMPP-Server")
    ResourceManager[Connection](
      resource = connection,
      close = _.close
    ).receiveWithin(1 minute)
    val comp = state.component.getOrElse_cps {
      log.info("Initializing XMPPComponent {}...", connection.from)
      val timeout = 5 minutes;
      ResourceManager[XMPPComponent](
        resource=spec.initializeComponent(connection.from, serverJid(connection), Manager).receiveWithin(timeout),
        close=_.shutdown).receiveWithin(timeout + (2 s)).resource
    }
    log.debug("The XMPPComponent is connecting")
    spawnChild(Required) {
      //execute in a different process because it won't be able to send
      // otherwise (Manager.send also runs in State-process)
      comp.connected
      cast { cs =>
        log.info("The XMPPComponent {} is now connected and ready", connection.from)
        startReader(cs)
      }
    }
    state.copy(connection=Some(connection), component=Some(comp))
  }
  protected[this] def connectionLost(state: State) = {
    log.info("Trying to reconnect to the XMPP-Server...")
    state.component.foreach_cps(_.connectionLost)
    openConnection
  }
  protected[this] def startReader(state: State): State @process = state.connection match {
    case Some(connection) => state.component match {
      case Some(component) =>
        state.reader.foreach_cps(_ ! Terminate)
        log.debug("Starting XMPP-Reader process for {}", connection.from)
        val reader = spawnChild(Monitored) {
          def run: Unit @process = receiveNoWait {
            case Timeout =>
              val read = connection.port.read()
              read match {
                case Data(data) =>
                  data.foreach_cps { _ match {
                    case r: IQResult if r.id.startsWith("ping-") =>
                      log.trace("Response to ping received")
                      noop
                    case d =>
                      log.trace("XMPPComponent receives {}", data)
                      component.process(d)
                  }}
                  run
                case EndOfData =>
                  cast { state =>
                    log.warn("XMPP-Connection for {} closed by the server", connection.from)
                    connectionLost(state)
                    state
                  }
              }
            case Terminate => ()
          }
          run
        }
        state.copy(reader=Some(reader))
      case None => state
    }
    case None => state
  }

  def sendKeepAlive = cast { state => state.connection match {
    case Some(connection) => 
      spawnChild(NotMonitored) {
        val jid = connection.from
        val ping = IQGet("ping-"+uniqueId, jid, serverJid(connection), <ping xmlns="urn:xmpp:ping"/>)
        connection.port.writeCast(ping)
      }
      state
    case None =>
      //no keep-alive if not connected
      state
  }}
  private[this] def serverJid(connection: Connection) = {
    val jid = connection.from
    val domain = jid.domain
    val server = {
      if (domain.startsWith(spec.subdomain+".")) domain.drop(spec.subdomain.length+1)
      else domain
    }
    JID(server)
  }
  protected[this] def uniqueId = java.util.UUID.randomUUID.toString

  def component = get(_.component)

  protected[this] val writeTimeout = 5 s

  protected[this] object Manager extends XMPPComponentManager {
    override def send(packet: XMPPPacket) = async { _.connection match {
      case Some(connection) => 
        connection.port.write(packet).receiveWithin(writeTimeout)
      case None =>
        log.warn("Component tried to send a packet but is not connected")
    }}
    override def unregister = stop
  }
}


case class StreamException(reasons: NodeSeq) extends Exception(reasons.map(_.label).mkString(", "))
case class XMPPException(text: String) extends Exception(text)

case class XMPPComponentHandshakeClient(secret: String, protected[this] val streamId: String, encoding: Charset)
     extends OtherXMPPPacket {
  import java.security._
  import scalabase.extcol.ListUtil._
  protected[this] def sha1Hash(of: String) = {
    val digest = MessageDigest.getInstance("SHA1")
    digest.update(of.getBytes(encoding))
    digest.digest
  }
  protected[this] def secretHash: String = {
    val source = streamId+secret
    val hashBinary = sha1Hash(source)
    hashBinary.view.map(_ & 0xFF).map(_.toHexString).map(s => if (s.length==1) "0"+s else s).mkString
  }
  def xml = <handshake xmlns="jabber:component:accept">{secretHash}</handshake>
}
object XMPPComponentHandshakeServer extends OtherXMPPPacket {
  val xml = <handshake xmlns="jabber:component:accept"/>
}
