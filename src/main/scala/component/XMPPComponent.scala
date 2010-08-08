package ch.inventsoft.scalaxmpp.component

import scala.xml._
import ch.inventsoft.scalabase.process._
import Messages._
import org.jivesoftware.whack.ExternalComponentManager
import ch.inventsoft.scalaxmpp._

trait XMPPComponentProvider {
  val name: String
  val description: String
  val subdomain: String
  def secret: String
  def initializeComponent(jid: JID, serverDomain: String, manager: XMPPComponentManager): MessageSelector[XMPPComponent]
}
trait XMPPComponent {
  def start: Unit
  def receive(packet: XMPPPacket): Unit
  def connectionLost: Unit
  def shutdown: Unit
}



trait XMPPComponentManager {
  /** Send an XMPP packet */
  def send(packet: XMPPPacket): Unit
  /** Removes the component */
  def unregister: Unit
  
  /** Get a server property */
  def get(key: String): MessageSelector[String]
  /** Set a configuration property on the server */
  def set(key: String, value: String): MessageSelector[Unit]

//TODO Log..
}


trait XMPPPacket {
  def toXml: Document
}

/**
 * Connection to an XMPP-Server
 */
class XMPPComponentServer(val host: String, val port: Int) {
  def this(host: String) = {
    this(host, 5275)
  }

  private[this] val manager = new ExternalComponentManager(host, port)

  def register(component: XMPPComponentProvider): Unit = { //TODO spawn
    //TODO
  }
}
