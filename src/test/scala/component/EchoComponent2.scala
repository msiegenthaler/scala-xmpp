package ch.inventsoft
package scalaxmpp
package component

import scala.xml._
import scalabase.process._
import scalabase.oip._
import scalabase.time._
import scalabase.log._


object EchoComponent2 extends Application with Log { spawnAndBlock {
  val server = XMPPComponentServer.tcp("localhost", 5275, None)

  class EchoAgent(val name: String, callback: AgentCallback) extends Agent with ConcurrentObject {
    protected[this] type State = Unit
    override def handleMessage(packet: MessagePacket) = concurrent { packet match {
      case MessageSend(_, "chat", from, _, content) =>
        content.find(_.label == "body").map(_.text).foreach_cps { text =>
          println("Echo "+name+" got message "+text)
          val c = <subject>Echo</subject><body>{name} says {text}</body>;
          callback.send(MessageSend(
            id=None,
            messageType="chat",
            from=callback.jid,
            to=from,
            content=c))
        }
      case other => //ignore
    }}
    override def handleIQ(packet: IQRequest) = concurrentWithReply {
      packet.resultError(StanzaError.badRequest)
    }
    override def handlePresence(packet: PresencePacket) = concurrent { packet match {
      case Presence(from, content, Some("subscribe"), _, id) =>
        console("got subscription request from "+from)
        callback.send(Presence(callback.jid, NodeSeq.Empty, Some("subscribed"), Some(from), id)).receive
      case Presence(from, content, Some("unsubscribed"), _, id) =>
        console(""+from+" unsubscribed")
      case Presence(from, content, Some("probe"), _, id) =>
        console("got probe from "+from)
        announce(Some(from)).receive
      case other =>
        //ignore
    }}
    protected[this] def announce(to: Option[JID]) = {
      val c = <show>chat</show><status>I'm ready to echo!</status>;
      callback.send(Presence(from=callback.jid, to=to, content=c))
    }
    protected[this] def announceOffline(to: Option[JID]) = {
      val c = <show>offline</show><status>I'm gone</status>;
      callback.send(Presence(from=callback.jid, to=to, content=c))
    }
    override def connected = concurrent {
      console("connected")
      announce(None).receive
    }
    override def connectionLost = {
      console("lost connection")
    }
    override def shutdown = concurrent {
      announceOffline(None).receive
      console("is shut down")
    }
    private def console(what: String) = {
      println("Echo "+name+" "+what)
    }
  }

  val spec = AgentComponent.specification("Echo", "Echos everything said the members", "echo2", Some("secret")) { am =>
    am.register(new EchoAgent("hans", _))
  }
  server.register(spec)
}}
