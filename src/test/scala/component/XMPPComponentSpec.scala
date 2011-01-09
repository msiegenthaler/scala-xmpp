package ch.inventsoft
package scalaxmpp
package component

import scala.xml._
import scala.collection.immutable.Queue
import org.scalatest._
import matchers._
import java.io._
import scalabase.process._
import scalabase.io._
import scalabase.oip._
import scalabase.time._
import scalabase.log._


class XMPPComponentSpec extends ProcessSpec with ShouldMatchers with Log {
  describe("XMPPComponentServer") {
    it_("should perform a proper handshake with the server") {
      val component = ComponentMock()
      val (c,s) = init
      val spec = component.specification
      val cmp = s.register(spec)

      val openElem = XML.loadString(c.read()+"</stream:stream>")
      openElem should be(<stream:stream to="test" xmlns:stream="http://etherx.jabber.org/streams"/>)
      c.send("""<?xml version="1.0" encoding="UTF-8"?>
<stream:stream id="1234" from="test.xmpp.inventsoft.ch" xmlns="jabber:component:accept" xmlns:stream="http://etherx.jabber.org/streams">""")

      val handshakeElem = XML.loadString(c.read())
      handshakeElem should be(<handshake xmlns="jabber:component:accept">c651141dcae96c2e1fc4edcd41b30a2b83f6be69</handshake>)
      notConnected(component)
      c.sendXml(<handshake/>)

      connected(component)
    }
  }

  def connected(c: ComponentMock) = {
    val con = c.isConnected.receiveWithin(200 ms)
    con should be(true)
  }
  def notConnected(c: ComponentMock) = {
    val con = c.isConnected.receiveWithin(200 ms)
    con should be(false)
  }

  object ComponentMock {
    def apply() = Spawner.start(new ComponentMock, SpawnAsRequiredChild)
  }
  class ComponentMock extends XMPPComponent with StateServer {
    val specification: XMPPComponentSpecification = {
      new XMPPComponentSpecification {
        override val name = "test component"
        override val description = "Just a test component"
        override val subdomain = "test"
        override val secret = Some("very-secret")
        override def initializeComponent(jid: JID, serverJid: JID, manager: XMPPComponentManager) = call { s =>
          jid should be(JID("test.xmpp.inventsoft.ch"))
          serverJid should be(JID("xmpp.inventsoft.ch"))                                                                                                          
          manager should not be(null)
          s.connected should be(false)
          s.manager should be(None)
          (ComponentMock.this, s.copy(manager=Some(manager)))
        }
      }
    }

    case class ComponentMockState(connected: Boolean=false, received: Queue[XMPPPacket]=Queue(), manager: Option[XMPPComponentManager]=None)
    override type State = ComponentMockState
    override def init = ComponentMockState()
    override def connected = cast { s =>
      s.connected should be(false)
      s.copy(connected=true)
    }
    override def connectionLost = cast { s =>
      s.connected should be(true)
      s.copy(connected=false)
    }
    def isConnected = get(_.connected)

    override def process(packet: XMPPPacket) = cast { s =>
      s.connected should be(true)
      s.copy(received = s.received enqueue packet)
    }
    
    override def shutdown = stopAndWait.receiveWithin(100 ms)
  }


  private def init = {
    val tpc = TestPortContainer()
    (tpc, mkserver(tpc.port))
  }
  private def mkserver(port: CommunicationPort[Byte,Byte]): XMPPComponentServer = {
    val cf = new ByteCPXMPPConnectionFactory {
      override def openRawConnection = port
    }
    val mgr = new BaseConnectionManager {
      override val connectionFactory = cf
      override val defaultSecret = None
    }
    new XMPPComponentServer with ConcurrentObject {
      override def register(componentSpec: XMPPComponentSpecification) = concurrentWithReply {
        val wrapper = new ComponentWrapper(componentSpec, mgr, 10 minutes)
        Spawner.start(wrapper, SpawnAsRequiredChild)
        ()
      }
    }
  }


  private class TestPortContainer {
    val deviceInput = new PipedInputStream
    val deviceOutput = new PipedOutputStream
    val portInput = new PipedInputStream(deviceOutput)
    val portOutput = new PipedOutputStream(deviceInput)
    val readBuffer = new StringBuilder

    def read(delay: Duration = (200 ms)) = {
      sleep(delay)
      readBuffer synchronized {
        val b = readBuffer.toString
        readBuffer.clear
        b
      }
    }
    
    def sendXml(elem: Elem) = {
      send(elem.toString)
    }
    def send(data: String, delay: Duration = (200 ms)): Unit @process = {
      log info ("Wrote to output "+data)
      sendBytes(data.getBytes("UTF-8").toList, delay)
    }
    def sendBytes(data: List[Byte], delay: Duration): Unit @process = {
      deviceOutput.write(data.toArray)
      deviceOutput.flush
      sleep(delay)
    }
    
    var _port: CommunicationPort[Byte,Byte] = null
    def port = {
      if (_port == null) throw new IllegalStateException
      else _port
    }
    var reader: Process = null

    def start: Unit @process = {
      _port = {
        case class SourceSink(source: Source[Byte], sink: Sink[Byte])
        CommunicationPort[Byte,Byte,SourceSink](
          open = {
            SourceSink(InputStreamSource(portInput), OutputStreamSink(portOutput))
          },
          close = { ss => 
            val s1 = ss.sink.close
            val s2 = ss.source.close
            s1.await; s2.await
          }
        ).receiveWithin(1 s)
      }
      reader = spawnChild(Required) {
        val decoder = java.nio.charset.Charset.forName("UTF-8").newDecoder()
        def read: Unit @process = {
          val a = deviceInput.available
          if (a > 0) {
            val b = java.nio.ByteBuffer.allocate(a)
            val r = deviceInput.read(b.array)
            if (r == -1) self ! Terminate
            else {
              b.position(r)
              b.flip
              val cb = java.nio.CharBuffer.allocate(a*2)
              decoder.decode(b, cb, false)
              cb.flip
              readBuffer synchronized {
                log.info("Read from input"+cb)
                readBuffer.append(cb.toString)
              }
            }
          }
          receiveWithin(10 ms) {
            case Terminate => ()
              case Timeout => read
          }
        }
        read
      }
    }
    
    def close = {
      if (reader != null)
        reader ! Terminate
      if (port != null)
        port.close
      deviceInput.close
      deviceOutput.close
    }
  }
  private object TestPortContainer {
    def apply() = {
      val o = new TestPortContainer
      o.start
      o
    }
  }
}
