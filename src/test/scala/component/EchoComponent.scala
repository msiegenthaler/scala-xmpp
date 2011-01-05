package ch.inventsoft
package scalaxmpp
package component

import scalabase.process._
import scalabase.oip._
import scalabase.time._
import scalabase.log._
import scala.xml._


object EchoComponent extends Application with Log { spawnAndBlock {
  val server = XMPPComponentServer.tcp("localhost", 5275, None)
  server.register(new EchoComponentSpecification)
  println("running")

  receive {
    case ProcessExit(_) =>
      log.info("Terminated.")
      println("Terminated")
    case ProcessKill(_, by, reason) =>
      log.error("Crashed ({})", by, reason)
      println("Crashed "+by)
      reason.printStackTrace
    case ProcessCrash(_, reason) =>
      log.error("Crashed", reason)
      println("Crashed")
      reason.printStackTrace
  }
}}


class EchoComponentSpecification extends XMPPComponentSpecification with ConcurrentObject {
  override val name = "echo"
  override val description = "Echos all messages sent to the component"
  override val subdomain = "echo"
  override val secret = Some("secret")

  override def initializeComponent(jid: JID, manager: XMPPComponentManager) = concurrentWithReply {
    new EchoComponent(this, jid, manager)
  }
}
class EchoComponent(spec: EchoComponentSpecification, id: JID, manager: XMPPComponentManager) extends XMPPComponent {
  override val specification = spec
  override def connected = {
    println("Conntected")
    announceHans(None)
  }
  override def connectionLost = {
    println("Disconnected")
  }
  override def shutdown = {
    println("Shutdown")
  }
  override def process(packet: XMPPPacket) = {

    packet match {
      case m: MessageSend =>
        if (m.to == hans) {
          m.content.find(_.label == "body").map(_.text) match {
            case Some(text) => 
              println("Echo "+text+" to "+m.from)
              text match {
                case "Shutdown" =>
                  manager.unregister
                case "Crash" => throw new RuntimeException("Requested crash")
                case text =>
                  msgHans(m.from, "Hans says: "+text)
              }
            case None =>
              println("No text message "+m)
          }
        } else println("Ignoring message "+m)
      case get: IQGet =>
        send(handleGet(get))
      case set: IQSet =>
        send(handleSet(set))
      case presence: Presence =>
        presence.presenceType match {
          case Some("subscribe") => 
            if (presence.to == Some(hans)) {
              println("Subscription to Hans from "+presence.from)
              send(Presence(hans, NodeSeq.Empty, Some("subscribed"), presence.fromOption, presence.id))
              send(Presence(hans, NodeSeq.Empty, Some("subscribe"), presence.fromOption))
            } else {
              println("Ignoring subscription for "+presence.to)
            }
          case Some("probe") =>
            if (presence.to == Some(hans)) announceHans(presence.fromOption)
            //TODO
          case other =>
            //ignore
              println("Ignoring presence for "+presence)
        }
      case other => //ignore unknown
        println("Received "+packet.xml)
    }
  }

  val hans = JID("hans", id.domain)
  def announceHans(to: Option[JID]) = {
    val c = <show>chat</show><status>Happy day!</status>;
    send(Presence(from=hans, to=to, content=c))
  }

  def handleGet(get: IQGet) = get.content.headOption match {
    case Some(e @ <query/>) if e.namespace=="http://jabber.org/protocol/disco#info" =>
      println("Discovery request")
      get.resultOk(<query xmlns="http://jabber.org/protocol/disco#items">
        <item jid={hans.stringRepresentation} />
      </query>)
    case other =>
      println("Unknown IQ-Get: "+get)
      get.resultError(unsupportedIQ)
  }
  def handleSet(set: IQSet) = set.content match {
    case other => set.resultError(unsupportedIQ)
  }

  private[this] def msgHans(to: JID, text: String) = {
    val c = <body>{text}</body>
    manager.send(MessageSend(id=None, messageType="chat",
      from=hans,
      to=to,
      content=c
    )).receiveWithin(1 s)
  }
  private[this] def send(packet: XMPPPacket): Unit @process = {
    println("Sending "+packet.getClass.getSimpleName)
    if (manager.send(packet).option.receiveWithin(20 s).isEmpty) {
      println("Failed to send "+packet.xml)
    }
    noop
  }
  private def unsupportedIQ = <bad-request xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
}
