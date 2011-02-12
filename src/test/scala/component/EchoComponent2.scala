package ch.inventsoft
package scalaxmpp
package component

import scala.xml._
import scalabase.process._
import scalabase.oip._
import scalabase.time._
import scalabase.log._

/*
object EchoComponent2 extends Application with Log { spawnAndBlock {
  val server = XMPPComponentServer.tcp("localhost", 5275, None)

  val store = java.util.prefs.Preferences.userRoot
  trait JIDStore {
    protected[this] val prefix: String
    protected[this] def key = prefix+".jids"
    def add(jid: JID) = {
      save(get :+ jid)
    }
    def remove(jid: JID) = {
      save(get.filter(_==jid))
    }
    protected[this] def save(list: Seq[JID]) = {
      val string = list.map(_.stringRepresentation).mkString(",")
      store.put(key, string)
    }
    def get: Seq[JID] = {
      val string = store.get(key, "")
      string.split(",").flatMap { e =>
        try {
          Some(JID.parse(e))
        } catch {
          case e: Exception => None
        }
      }
    }
  }

  class EchoAgent(callback: AgentCallback) extends Agent with ConcurrentObject {
    val name = callback.jid.node.getOrElse("")
    val subscribers = new JIDStore { override val prefix = name }
    protected[this] type State = Unit
    override def handleMessage(packet: MessagePacket) = concurrent { packet match {
      case MessageSend(_, "chat", from, _, content) =>
        content.find(_.label == "body").map(_.text).foreach_cps { text =>
          println("Echo "+name+" got message "+text)
          text match {
            case "stop" => callback.unregister
            case "offline" => announceOffline()
            case "online" => announce()
            case text =>
              val c = <subject>Echo</subject><body>{name} says {text}</body>;
              callback.send(MessageSend(
                id=None,
                messageType="chat",
                from=callback.jid,
                to=from,
                content=c))
          }
        }
      case other => //ignore
    }}
    override def handleIQ(packet: IQRequest) = concurrentWithReply {
      packet.resultError(StanzaError.badRequest)
    }
    override def handlePresence(packet: PresencePacket) = concurrent { packet match {
      case Presence(from, content, Some("subscribe"), _, _) =>
        console("got subscription request from "+from)
        addSubscription(from)
      case Presence(from, content, Some("unsubscribed"), _, id) =>
        console(""+from+" unsubscribed")
        removeSubscription(from)
      case Presence(from, content, Some("probe"), _, id) =>
        console("got probe from "+from)
        announce(Some(from))
      case other =>
        //ignore
    }}
    override def connected = concurrent {
      console("connected")
      announce(None)
    }
    override def connectionLost = {
      console("lost connection")
    }
    override def shutdown = concurrent {
      announceOffline(None)
      console("is shut down")
    }
    private def console(what: String) = {
      println("Echo "+name+" "+what)
    }

    protected[this] def announce(to: Option[JID]=None): Unit @process = {
      val status = <show>chat</show><status>Ready to echo!</status>;
      announce(to, None, status)
    }
    protected[this] def announceOffline(to: Option[JID]=None) = {
      val status = <status>I'm gone</status>;
      announce(to, Some("unavailable"), status)
    }
    protected[this] def announce(to: Option[JID], kind: Option[String], status: NodeSeq): Unit @process = to match {
      case Some(_) =>
        callback.send(Presence(from=callback.jid, to=to, content=status, presenceType=kind)).receiveOption(1 s)
        ()
      case None =>
        console("Announcing presence to all subscribers")
        val selectors = subscribers.get.map_cps { to =>
          console(" informs "+to)
          callback.send(Presence(from=callback.jid, to=Some(to), content=status, presenceType=kind))
        }
        selectors.foreach_cps(_.receiveOption(1 s))
    }
    protected[this] def addSubscription(jid: JID) = {
      callback.send(Presence(callback.jid, NodeSeq.Empty, Some("subscribed"), Some(jid))).receive
      subscribers.add(jid)
      callback.send(Presence(callback.jid, NodeSeq.Empty, Some("subscribe"), Some(jid))).receive
    }
    protected[this] def removeSubscription(jid: JID) = {
      subscribers.remove(jid)
    }
  }

  val spec = AgentComponent.specification("Echo", "Echos everything said the members", "echo2", Some("secret")) { am =>
    am.register("hans", new EchoAgent(_))
  }
  server.register(spec)
}}
*/
