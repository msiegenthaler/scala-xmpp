package ch.inventsoft
package scalaxmpp

import scala.xml._
import XmlSupport._


sealed trait XMPPPacket {
  def xml: Elem
}

//TODO xml:lang
trait Stanza extends XMPPPacket {
  def from: JID
  def to: JID
  def stanzaType: String
  def content: NodeSeq
}
trait StanzaError extends XMPPPacket {
//TODO
}


trait MessagePacket extends Stanza {
  def id: Option[String]
}
case class MessageSend(id: Option[String], messageType: String, from: JID, to: JID, content: NodeSeq) extends MessagePacket {
  override def stanzaType = messageType
  override def xml = {
    val m = <message type={messageType} from={from.stringRepresentation} to={to.stringRepresentation}>{content}</message>
    m.optAttr("id", id)
  }
}
//TODO error

trait IQPacket extends Stanza {
  def id: String
}
case class IQGet(id: String, from: JID, to: JID, content: NodeSeq) extends IQPacket {
  override val stanzaType = "get"
  override def xml = {
    <iq type={stanzaType} from={from.stringRepresentation} to={to.stringRepresentation} id={id}>{content}</iq>
  }
  def result(content: NodeSeq) = IQResult(id, to, from, content)
//TODO error
/*
  def error(error: NodeSeq, includeRequest: Boolean = true) = {
    val c = if (includeRequest) error ++ content
            else content
    IQError(id, to, from, content)
  }
  */
}
case class IQSet(id: String, from: JID, to: JID, content: NodeSeq) extends IQPacket {
  override val stanzaType = "set"
  override def xml = {
    <iq type={stanzaType} from={from.stringRepresentation} to={to.stringRepresentation} id={id}>{content}</iq>
  }
}
case class IQResult(id: String, from: JID, to: JID, content: NodeSeq) extends IQPacket {
  override val stanzaType = "result"
  override def xml = {
    <iq type={stanzaType} from={from.stringRepresentation} to={to.stringRepresentation} id={id}>{content}</iq>
  }
}
//TODO
//case class IQError(id: String, from: JID, to: JID, content: NodeSeq)


case class Presence(from: JID, content: NodeSeq, presenceType: Option[String]=None, to: Option[JID]=None, id: Option[String]=None) extends XMPPPacket {
  override def xml = {
    val p = <presence from={from.stringRepresentation}>{content}</presence>
    p.optAttr("to", to.map(_.stringRepresentation))
     .optAttr("type", presenceType)
     .optAttr("id", id)
  }
}


trait StreamErrorPacket extends XMPPPacket {
  def content: Seq[Elem]
//TODO
}

trait OtherXMPPPacket extends XMPPPacket {
}

object OtherXMPPPacket {
  def apply(content: Elem) = new OtherXMPPPacket {
    val xml = content
  }
  def unapply(packet: OtherXMPPPacket) = Some(packet.xml)
}

trait StreamAwareXMPPPacket {
  def stream: Elem
}



object XMPPPacket {
  def apply(elem: Elem): XMPPPacket = elem match {
    case <iq>{content @ _*}</iq> =>
      val to = parseJid(elem \ "@to")
      val from = parseJid(elem \ "@from")
      val id = (elem \ "@id").toString
      (elem \ "@type").toString match {
        case "get" =>
          IQGet(id, from, to, content)
        case "set" =>
          IQSet(id, from, to, content)
        case "result" =>
          IQResult(id, from, to, content)
        case other => null //TODO
      }
    case <message>{content @ _*}</message> =>
      val to = parseJid(elem \ "@to")
      val from = parseJid(elem \ "@from")
      val id = elem.attribute("id").map(_.toString)
      val t = (elem \ "@type").toString
      MessageSend(id, t, from, to, content)
//TODO error
    case <presence>{content @ _*}</presence> =>
      val to = elem.attribute("to").map(parseJid(_))
      val from = parseJid(elem \ "@from")
      val id = elem.attribute("id").map(_.toString)
      val t = elem.attribute("type").map(_.toString)
      Presence(from, content, t, to, id)
                     
    case elem =>
      new OtherXMPPPacket {
        override val xml = elem
      }
  }

  private def parseJid(data: NodeSeq): JID = {
    val text = data.toString
    JID.parse(text)
  }

/*
  def convert(from: org.xmpp.packet.Packet): XMPPPacket = {
    null //TODO
  }
  def convert(from: XMPPPacket): org.xmpp.packet.Packet = {
    val xml = from.toXml
    //TODO reparse from xml..
//    new org.xmpp.packet.Packet()
    null //TODO
  }

  implicit def xmppToXml(packet: XMPPPacket): Elem = packet.toXml
  */
}


private object XmlSupport {
  class ExtElem(elem: Elem) {
    def attr(key: String, value: String): Elem = {
      elem % Attribute(key, Text(value), Null)
    }
    def optAttr(key: String, value: Option[String]): Elem = value match {
      case Some(value) =>
        elem % Attribute(key, Text(value), Null)
      case None => elem
    }
  }
  implicit def extElemWrapper(elem: Elem): ExtElem = new ExtElem(elem)
}
