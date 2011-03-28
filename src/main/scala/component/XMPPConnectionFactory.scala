package ch.inventsoft
package scalaxmpp
package component

import scala.xml._
import java.nio.charset.Charset
import java.io._
import java.net._
import scalabase.process._
import Messages._
import scalabase.io._
import scalabase.oip._


/**
 * Creates connections to an XMPP-Server
 */
trait XMPPConnectionFactory {
  /** opens a new connection to the server */
  def openConnection(
    outgoingRoot: Elem,
    as: SpawnStrategy = SpawnAsRequiredChild): Selector[CommunicationPort[XMPPPacket,XMPPPacket]] @process
}


trait ByteCPXMPPConnectionFactory extends XMPPConnectionFactory {
  /** charset for decoding/encoding xml to bytes */
  val encoding: Charset = Charset.forName("UTF-8")

  protected def openRawConnection: CommunicationPort[Byte,Byte] @process

  protected def open(outgoingRoot: Elem): ConnectionObject @process = {
    val raw = openRawConnection
    val xmlSource = XmlChunkSource.fromBytes(byteSource=raw, encoding=encoding, sendRoot=true)
    val xmlSink = XmlChunkSink.withRoot(outgoingRoot).outputingBytesTo(raw, encoding)()
    val xmppSource = new OneToOneTransformingSource[Elem,XMPPPacket] {
      override val source = xmlSource
      override def transform(elem: Elem) = XMPPPacket(elem)
      override def toString = "XmppSource"
    }
    val xmppSink = new OneToOneTransformingSink[XMPPPacket,Elem] {
      override val sink = xmlSink
      override def transform(packet: XMPPPacket) = packet.xml
      override def toString = "XmppSink"
    }
    new ConnectionObject {
      override val source = xmppSource
      override val sink = xmppSink
    }
  }
  protected def close(conObj: ConnectionObject): Unit @process = {
    conObj.close
  }

  override def openConnection(outgoingRoot: Elem, as: SpawnStrategy = SpawnAsRequiredChild) = {
    CommunicationPort[XMPPPacket,XMPPPacket,ConnectionObject](
      open = open(outgoingRoot),
      close = close _,
      as = as
    )
  }

  protected trait ConnectionObject {
    val source: Source[XMPPPacket]
    val sink: Sink[XMPPPacket]
    def close: Unit @process = {
      sink.close.await
      source.close.await
    }
  }
}

/** XMPP-Connection Factory for java.net.Socket based communication */
trait SocketXMPPConnectionFactory extends ByteCPXMPPConnectionFactory {
  /** Hostname */
  val host: String
  /** TCP Port */
  val port: Int

  protected override def openRawConnection = {
    CommunicationPort[Byte,Byte,SocketConnection](
      open = openSocket,
      close = closeSocket(_)
    ).receive
  }
  protected def openSocket = {
    val socket = new Socket(host, port)
    val source = InputStreamSource(socket.getInputStream)
    val sink = OutputStreamSink(socket.getOutputStream)
    new SocketConnection(socket, source, sink)
  }
  protected def closeSocket(sc: SocketConnection) = {
    sc.source.close.await
    sc.sink.close.await
    sc.socket.close
  }

  protected case class SocketConnection(socket: Socket, source: Source[Byte], sink: Sink[Byte])
}
