package ch.inventsoft
package scalaxmpp

import scala.xml._
import XmlSupport._


sealed trait XMPPPacket {
  def xml: Elem
}

//TODO add xml:lang somewhen
trait Stanza extends XMPPPacket {
  def isError: Boolean
  def fromOption: Option[JID]
  def toOption: Option[JID]
  def idOption: Option[String]
  def stanzaType: String
  def content: NodeSeq
}
trait StanzaError extends Stanza {
  override val stanzaType = "error"
  override def isError = true
  def errorElem: Elem
  def errorType: String = errorElem.attribute("type").map(_.toString).getOrElse("")
  def errorText: Option[String] = errorTextElem.map(_.text)
  def errorTextElem: Option[Elem] = {
    val e = errorElem.child.filter { e => 
        (e.label == "text") && (e.scope.uri == "urn:ietf:params:xml:ns:xmpp-stanzas")
      }.headOption
    if (e.isEmpty) None
    else e match {
      case e: Elem => Some(e)
      case _ => None
    }
  }
  def errorCondition: Option[Elem] =
    errorElem.child.filter(_.isInstanceOf[Elem]).map(_.asInstanceOf[Elem]).headOption

  /*
  <stanza-kind to='sender' type='error'>
   [RECOMMENDED to include sender XML here]
   <error type='error-type'>
     <defined-condition xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
     <text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas' xml:lang='langcode'>
       OPTIONAL descriptive text
     </text>
     [OPTIONAL application-specific condition element]
   </error>
  </stanza-kind>
  */
}
object StanzaError {
  val badRequest = <error type="modify"><bad-request xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val conflict = <error type="wait"><conflict xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val featureNotImplemented = <error type="cancel"><feature-not-implemented xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val forbidden = <error type="auth"><conflict xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val gone = <error type="modify"><gone xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val internalServerError = <error type="wait"><internal-server-error xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val itemNotFound = <error type="cancel"><item-not-found xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val jidMalformed = <error type="modify"><jid-malformed xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val notAcceptable = <error type="modify"><not-acceptable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val notAllowed = <error type="cancel"><not-allowed xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val notAuthorized = <error type="auth"><not-authorized xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val paymentRequired = <error type="auth"><payment-required xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val recipientUnavailable = <error type="wait"><recipient-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val redirect = <error type="modify"><redirect xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val registrationRequired = <error type="wait"><registration-required xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val remoteServerNotFound = <error type="cancel"><remote-server-not-found xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val remoteServerTimeout = <error type="wait"><remote-server-timeout xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val resourceConstraint = <error type="wait"><resource-constraint xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val serviceUnavailable = <error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val subscriptionRequired = <error type="auth"><subscription-required xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
  val unexpectedRequest = <error type="wait"><unexpected-request xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>
}

sealed trait MessagePacket extends Stanza {
  def id: Option[String]
  def to: JID
  override def toOption = Some(to)
  def from: JID
  override def fromOption = Some(from)
}
case class MessageSend(id: Option[String]=None, messageType: String, from: JID, to: JID, content: NodeSeq) extends MessagePacket {
  override def isError = false
  override def stanzaType = messageType
  override def idOption = id
  override def xml = {
    val m = <message type={messageType} from={from.stringRepresentation} to={to.stringRepresentation}>{content}</message>
    m.optAttr("id", id)
  }
}
case class MessageError(id: Option[String], from: JID, to: JID, errorElem: Elem, otherContent: NodeSeq = NodeSeq.Empty) extends MessagePacket with StanzaError {
  override def idOption = id
  override def toOption = Some(to)
  override def content = {
    otherContent ++ errorElem
  }
  override def xml = {
    val m = <message type="error" from={from.stringRepresentation} to={to.stringRepresentation}>{content}</message>
    m.optAttr("id", id)
  }
}

sealed trait IQPacket extends Stanza {
  def from: JID
  def to: JID
  def id: String
  override def fromOption = Some(from)
  override def toOption = Some(to)
  override def idOption = Some(id)
}
sealed trait IQRequest extends IQPacket {
  def resultOk(content: NodeSeq) = {
    IQResult(id, to, from, content)
  }
  def resultError(error: Elem, includeRequest: Boolean=true) = {
    IQError(id, to, from, error, if (includeRequest) content else NodeSeq.Empty)
  }
}
case class IQGet(id: String, from: JID, to: JID, content: NodeSeq) extends IQRequest {
  override val stanzaType = "get"
  override def isError = false
  override def xml = {
    <iq type={stanzaType} from={from.stringRepresentation} to={to.stringRepresentation} id={id}>{content}</iq>
  }
}
case class IQSet(id: String, from: JID, to: JID, content: NodeSeq) extends IQRequest {
  override val stanzaType = "set"
  override def isError = false
  override def xml = {
    <iq type={stanzaType} from={from.stringRepresentation} to={to.stringRepresentation} id={id}>{content}</iq>
  }
}
sealed trait IQResponse extends IQPacket
case class IQResult(id: String, from: JID, to: JID, content: NodeSeq) extends IQResponse {
  override val stanzaType = "result"
  override def isError = false
  override def xml = {
    <iq type={stanzaType} from={from.stringRepresentation} to={to.stringRepresentation} id={id}>{content}</iq>
  }
}
case class IQError(id: String, from: JID, to: JID, errorElem: Elem, otherContent: NodeSeq=NodeSeq.Empty) extends IQResponse with StanzaError {
  override val stanzaType = "error"
  override def content = otherContent ++ errorElem
  override def xml = {
    <iq type={stanzaType} from={from.stringRepresentation} to={to.stringRepresentation} id={id}>{content}</iq>
  }
}

trait PresencePacket extends Stanza {
  def from: JID
  def fromOption = Some(from)
  def presenceType: Option[String]
}
case class Presence(from: JID, content: NodeSeq, presenceType: Option[String]=None, to: Option[JID]=None, id: Option[String]=None) extends PresencePacket {
  override val stanzaType = "presence"
  override def isError = false
  override def fromOption = Some(from)
  override def toOption = to
  override def idOption = id
  override def xml = {
    val p = <presence from={from.stringRepresentation}>{content}</presence>
    p.optAttr("to", to.map(_.stringRepresentation))
     .optAttr("type", presenceType)
     .optAttr("id", id)
  }
}
case class PresenceError(from: JID, errorElem: Elem, id: Option[String]=None, to: Option[JID]=None, otherContent: NodeSeq=NodeSeq.Empty) extends PresencePacket with StanzaError {
  override val presenceType = Some("error")
  override def content = otherContent ++ errorElem
  override def idOption = id
  override def toOption = to
  override def xml = {
    val p = <presence from={from.stringRepresentation} type="error">{content}</presence>
    p.optAttr("to", to.map(_.stringRepresentation))
     .optAttr("id", id)
  }
}


case class StreamError(content: NodeSeq) extends XMPPPacket {
  def xml = <stream:error xmlns:stream="http://etherx.jabber.org/streams">{content}</stream:error>
}

trait OtherXMPPPacket extends XMPPPacket {
}

object OtherXMPPPacket {
  def apply(content: Elem) = new OtherXMPPPacket {
    val xml = content
  }
  def unapply(packet: OtherXMPPPacket) = Some(packet.xml)
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
        case "error" =>
          elem.child.find(_.label == "error") match {
            case Some(error: Elem) =>
              val other = elem.child.filter(_.label != "error")
              IQError(id, from, to, error, other)
            case other => OtherXMPPPacket(elem)
          }
        case other => OtherXMPPPacket(elem)
      }

    case <message>{content @ _*}</message> =>
      val to = parseJid(elem \ "@to")
      val from = parseJid(elem \ "@from")
      val id = elem.attribute("id").map(_.toString)
      val t = (elem \ "@type").toString
      if (t == "error") {
        elem.child.find(_.label == "error") match {
          case Some(error: Elem) =>
            val others = elem.child.filter(_.label != "error")
            MessageError(id, from, to, error, others)
          case other => OtherXMPPPacket(elem)
        }
      } else MessageSend(id, t, from, to, content)

    case <presence>{content @ _*}</presence> =>
      val to = elem.attribute("to").map(parseJid(_))
      val from = parseJid(elem \ "@from")
      val id = elem.attribute("id").map(_.toString)
      val t = elem.attribute("type").map(_.toString)
      if (t == Some("error")) {
        elem.child.find(_.label == "error") match {
          case Some(error: Elem) =>
            val others = elem.child.filter(_.label != "error")
            PresenceError(from, error, id, to, others)
          case other => OtherXMPPPacket(elem)
        }
      } else Presence(from, content, t, to, id)

    case <error>{content @ _*}</error> =>
      StreamError(content)
                     
    case elem =>
      OtherXMPPPacket(elem)
  }

  private def parseJid(data: NodeSeq): JID = {
    val text = data.toString
    JID.parse(text)
  }
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
