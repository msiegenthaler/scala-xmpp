package ch.inventsoft.scalaxmpp.component

import scala.xml._
import ch.inventsoft.scalabase.process._
import Messages._
import ch.inventsoft.scalabase.log._
import ch.inventsoft.scalabase.oip._
import org.jivesoftware.whack.ExternalComponentManager
import ch.inventsoft.scalaxmpp._
import ch.inventsoft.scalabase.executionqueue.ExecutionQueues._


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
  /** Create the new XMPPComponent. Is called by XMPPComponentServer#add. */
  protected[component] def initializeComponent(jid: JID, serverDomain: String, manager: XMPPComponentManager): MessageSelector[XMPPComponent]
}

/**
 * XMPP component as per XEP-0114. Component enhance the functionality of an XMPP server.
 * The all methods must be implemented as very short running and non-blocking. Proposed implementation
 * is based upon processes (either a StateServer or by spawning a process per method invocation.
 */
trait XMPPComponent {
  val specification: XMPPComponentSpecification
  protected[this] val manager: XMPPComponentManager

  /** Start the component. All methods offered by the manager can be used. */
  def start: Unit

  /** A packet for the component. */
  def receive(packet: XMPPPacket): Unit

  /** The connection to the server has been lost. The manager will try to reestablish the connection forever. */
  def connectionLost: Unit
  /** Shut down the component and release all resources. */
  def shutdown: Unit
}


/**
 * Manager for a (single) XMPP component. The 
 */
trait XMPPComponentManager {
  /** Send an XMPP packet */
  def send(packet: XMPPPacket): Unit @processCps
  /** Removes the component */
  def unregister: Unit @processCps
  
  /** Get a server property */
  def get(key: String): MessageSelector[String] @processCps
  /** Set a configuration property on the server */
  def set(key: String, value: String): MessageSelector[Unit] @processCps
}


trait XMPPPacket {
  def toXml: Document
}
object XMPPPacket {
  def convert(from: org.xmpp.packet.Packet): XMPPPacket = {
    null //TODO
  }
  def convert(from: XMPPPacket): org.xmpp.packet.Packet = {
    null //TODO
  }
}

/**
 * Connection to an XMPP-Server
 */
class XMPPComponentServer(val host: String, val port: Int, val defaultSecret: Option[String]) {
  def this(host: String) = {
    this(host, 5275, None)
  }

  private[this] val manager = new ExternalComponentManager(host, port) //TODO try to override processMessage on external component because it needlessly uses a thread pool...

  def add(spec: XMPPComponentSpecification): MessageSelector[XMPPComponent] @processCps = {
    val c = WrappedComponent(spec)
    c.waitForInitialization
  }

  protected class WrappedComponent(spec: XMPPComponentSpecification) extends org.xmpp.component.Component with StateServer {
    type State = WrappedComponentState
    protected[this] override def init = {
      spec.secret.foreach(manager.setSecretKey(spec.subdomain, _))
      manager.addComponent(spec.subdomain, this)
      WrappedComponentState(None, Nil)
    }
    override def getName = spec.name
    override def getDescription = spec.description
    def waitForInitialization = {
      this ! new ModifyStateMessage with MessageWithSimpleReply[XMPPComponent] {
        override def execute(state: State) = state.component match {
          case Some(c) =>
            replyValue(c)
            state
          case None =>
            state.copy(notifyOnInit=replyValue _ :: state.notifyOnInit)
        }
      }
    }
    override def initialize(j: org.xmpp.packet.JID, manager: org.xmpp.component.ComponentManager) = cast { state =>
      val jid = JID.parse(j.toString)
      val mgr = new WrappedComponentManager(spec, this, manager)
      val sd = manager.getServerName
      val c = receive { spec.initializeComponent(jid, sd, mgr) }
      state.notifyOnInit.foreach(fun => fun(c))
      state.copy(component=Some(c), notifyOnInit=Nil)
    }
    override def start = withComponent { c =>
      c.start
    }
    override def processPacket(packet: org.xmpp.packet.Packet) = withComponent { c =>
      val p: XMPPPacket = XMPPPacket.convert(packet)
      c.receive(p)
    }
    override def shutdown = withComponent { c =>
      c.shutdown
    }

    protected[this] def withComponent(fun: XMPPComponent => Unit @processCps) = cast { state => state.component match {
      case Some(c) => fun(c); state
      case None => throw new IllegalStateException("Cannot start if not initialized")
    }}
  }
  protected object WrappedComponent extends SpawnableCompanion[WrappedComponent] {
    def apply(spec: XMPPComponentSpecification) = {
      start(SpawnAsRequiredChild)(new WrappedComponent(spec))
    }
  }
  protected case class WrappedComponentState(component: Option[XMPPComponent], notifyOnInit: List[Function1[XMPPComponent,Unit]])

  protected class WrappedComponentManager(spec: XMPPComponentSpecification, component: org.xmpp.component.Component, manager: org.xmpp.component.ComponentManager) extends XMPPComponentManager with ConcurrentObject { //TODO
    protected[this] override def concurrentQueue = executeForBlocking
    override def send(packet: XMPPPacket) = concurrent {
      val p: org.xmpp.packet.Packet = XMPPPacket.convert(packet)
      manager.sendPacket(component, p)
    }
    override def unregister = concurrent {
      manager.removeComponent(spec.subdomain)
    }
    override def get(key: String) = concurrentWithReply {
      manager.getProperty(key)
    }
    override def set(key: String, value: String) = concurrentWithReply {
      manager.setProperty(key, value)
    }
  }
}
