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
  /** Send an XMPP packet */
  def send(packet: XMPPPacket): Completion @process

  /** Removes the component */
  def unregister: Unit @process
  
  /** Get a server property */
  def get(key: String): Selector[String] @process
  /** Set a configuration property on the server */
  def set(key: String, value: String): Completion @process
}


/**
 * A (remote)  XMPP-Server.
 */
trait XMPPComponentServer {
  val host: String
//  val port: Int TODO

  /**
   * Register a new XMPP-Component for the server.
   */
  def register(componentSpec: XMPPComponentSpecification): Selector[XMPPComponent] @process
}

//TODO Name (impl sucks)
trait XMPPComponentServerImpl extends XMPPComponentServer with ConcurrentObject {
  protected[this] val defaultSecret: Option[String]

  protected[this] def connectionFactory: XMPPConnectionFactory

  /** Open a new connection to the xmpp-server */
  protected[this] def openConnection: CommunicationPort[XMPPPacket,XMPPPacket] @process = {
    connectionFactory.openConnection(clientRoot).receiveWithin(5 s)
  }

  protected[this] val clientRoot: Elem = {
    <stream:stream xmlns="jabber:component:accept"
                   xmlns:stream="http://etherx.jabber.org/streams"
                   to={host}/>
  }

  override def register(specification: XMPPComponentSpecification) = concurrentWithReply {
    //TODO supervisor process (restart etc. instead of required child)

    val wrapper = new XMPPComponentWrapper {
      override val componentSpec = specification
      override val subdomain = specification.subdomain
      override val encoding = Charset.forName("UTF-8")
      override def openConnection = XMPPComponentServerImpl.this.openConnection
    }
    Spawner.start(wrapper, SpawnAsRequiredChild)
    wrapper.component.receive
  }
}

trait XMPPComponentWrapper extends StateServer {
  val componentSpec: XMPPComponentSpecification
  val subdomain: String

  protected[this] val encoding: Charset
  protected[this] val defaultSecret: Option[String] = None
  protected[this] def openConnection: CommunicationPort[XMPPPacket,XMPPPacket] @process

  protected[this] val timeout = 30 s

  protected[this] override type State = WrapperState
  protected[this] case class WrapperState(component: XMPPComponent,
                                          connection: CommunicationPort[XMPPPacket,XMPPPacket],
                                          reader: Process)
  override def init = {
    val (conn, stream) = initConnection
    val from = (stream \ "@from").text
    val domain = subdomain+"."+from
    val component = initComponent(JID(domain))
    val reader = spawnChild(Required)(readFromServer(conn, component))
    WrapperState(component, conn, reader)
  }
  protected[this] def secret = componentSpec.secret.getOrElse(defaultSecret.getOrElse(""))
  protected[this] def initConnection = {
    val conn = openConnection
    val stream = receiveStreamBegin(conn)
    performHandshake(conn, stream)
    (conn, stream)
  }
  protected[this] def receiveStreamBegin(connection: Source[XMPPPacket]) = {
    val read: Read[XMPPPacket] = connection.readWithin(timeout, 1).getOrElse {
      throw new XMPPException("No answer from server (expected a <stream>)")
    }
    read match {
      case Data(packets) => packets.head match {
        case OtherXMPPPacket(data)
            if data.namespace=="http://etherx.jabber.org/streams" && data.label=="stream" =>
          noop
          data
        case other =>
          throw XMPPException("Unexpected element "+other.xml)
      }
      case EndOfData => throw XMPPException("Server closed the connection")
    }
  }
  protected[this] def performHandshake(connection: CommunicationPort[XMPPPacket,XMPPPacket], stream: Elem) = {
    val streamId = (stream \ "@id").text
    val handshake = XMPPComponentHandshakeClient(secret, streamId, encoding)
    connection.write(handshake).await(timeout)
    val response = connection.readWithin(timeout, 1).
      getOrElse(throw new XMPPException("No answer received from server (handshake)"))
    val data = response match {
      case Data(data) => data
      case EndOfData =>
        throw new XMPPException("server closed the connection")
    }
    data.head match {
      case p @ OtherXMPPPacket(XMPPComponentHandshakeServer.xml) =>
        //ok, server accepted the secret
        ()
      case error: StreamError =>
        throw new StreamException(error.content)
      case other =>
        throw new XMPPException("Received unexpected handshake answer: "+other)
    }
    data.tail //TODO handle the tail!
  }

  protected[this] def initComponent(jid: JID) = {
    val c = componentSpec.initializeComponent(jid, Manager).receiveWithin(30 s)
    c.connected
    c
  }
  protected[this] def readFromServer(source: Source[XMPPPacket], component: XMPPComponent): Unit @process = {
    val read = source.read()
    read match {
      case Data(packets) =>
        packets.foreach_cps(processIn(component, _))
        readFromServer(source, component)
      case EndOfData => noop
    }
  }
  protected[this] def processIn(component: XMPPComponent, packet: XMPPPacket) = {
    //TODO handle responses to get
    component.process(packet)
  }

  def component = get(_.component)

//TODO component stop
//TODO connection lost

//TODO keep-alive

  protected[this] object Manager extends XMPPComponentManager {
    override def send(packet: XMPPPacket) = call { s =>
      s.connection.write(packet).receiveWithin(timeout)
      ((), s)
    }
    override def set(key: String, value: String) = {
      //TODO
      null
    }
    override def get(key: String) = {
      //TODO
      null
    }

    override def unregister = {
      //TODO
    }
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
