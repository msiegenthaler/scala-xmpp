package ch.inventsoft
package scalaxmpp

import scala.xml._


//TODO
sealed trait XMPPPacket {
  def xml: Elem
}

trait IQPacket extends XMPPPacket {
  //TODO
}

trait PresencePacket extends XMPPPacket {
  //TODO
}

trait MessagePacket extends XMPPPacket {
  //TODO
}

trait StreamErrorPacket extends XMPPPacket {
  def content: Seq[Elem]
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
  def apply(elem: Elem): XMPPPacket = {
    //TODO
    new OtherXMPPPacket {
      override val xml = elem
    }
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
