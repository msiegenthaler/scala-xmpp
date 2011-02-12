package ch.inventsoft
package scalaxmpp
package component

import scala.xml._
import scalabase.process._
import scalabase.time._
import scalabase.oip._
import scalabase.log._
import Messages._


trait StatelessAgent extends Agent with ConcurrentObject {
  override def handleIQ(packet: IQRequest) = concurrentWithReply {
    packet match {
      case get: IQGet =>
        cpsOption(handle(iqGet)(get)).getOrElse_cps {
          packet.resultError(StanzaError.badRequest)
        }
      case set: IQSet =>
        cpsOption(handle(iqSet)(set)).getOrElse_cps {
          packet.resultError(StanzaError.badRequest)
        }
    }
  }
  override def handleMessage(packet: MessagePacket) = concurrent {
    handleNoResult(message)(packet)
    noop
  }
  override def handlePresence(packet: PresencePacket) = concurrent {
    handleNoResult(presence)(packet)
    noop
  }
  override def handleOther(packet: XMPPPacket) = concurrent {
    handleNoResult(other)(packet)
    noop
  }

  private def handleNoResult[X](handler: Traversable[PartialFunction[X,_ @process]])(value: X): Unit @process =
    handler.find(_.isDefinedAt(value)).foreach_cps(_.apply(value))
  private def handle[X,R](handler: Traversable[PartialFunction[X,R @process]])(value: X): Option[R] @process =
    handler.find(_.isDefinedAt(value)).map_cps(_.apply(value))

  protected def iqGet: Seq[PartialFunction[IQGet,IQResponse @process]] = Nil
  protected def iqSet: Seq[PartialFunction[IQSet,IQResponse @process]] = Nil
  protected def message: Seq[PartialFunction[MessagePacket,_ @process]] = Nil
  protected def presence: Seq[PartialFunction[PresencePacket,_ @process]] = Nil
  protected def other: Seq[PartialFunction[XMPPPacket,_ @process]] = Nil

  protected def mkIqGet[R](fun: PartialFunction[IQGet,IQResponse @process]) = fun
  protected def mkIqSet[R](fun: PartialFunction[IQGet,IQResponse @process]) = fun
  protected def mkMsg[R](fun: PartialFunction[MessagePacket,_ @process]) = fun
  protected def mkPr[R](fun: PartialFunction[PresencePacket,_ @process]) = fun
  protected def mkOth[R](fun: PartialFunction[XMPPPacket,_ @process]) = fun
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
  /** (Subject, Body, Sender) */
  def unapply(msg: MessagePacket) = msg match {
    case MessageSend(_, "chat", from, _, content) =>
      for {
        body <- content.find(_.label == "body")
        subject <- content.find(_.label == "subject").map(_.text)
      } yield (subject, body, from)
    case _ => None
  }
  def apply(subject: String, body: NodeSeq, to: JID, from: JID) = {
    val c = <subject>{subject}</subject><body>{body}</body>;
    MessageSend(
      id=None,
      messageType="chat",
      from=from,
      to=to,
      content=c
    )
  }
}
