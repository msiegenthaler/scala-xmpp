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



  val store = java.util.prefs.Preferences.userRoot
  trait JIDStore {
    protected val prefix: String
    protected def key = prefix+".jids"
    def add(jid: JID) = save(get :+ jid)
    def remove(jid: JID) = save(get.filter(_==jid))
    def set(list: Seq[JID]) = save(list)
    protected def save(list: Seq[JID]) = {
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




  class EchoAgent(override protected val services: AgentServices) extends PresenceManager {
    protected val name = services.jid.node.getOrElse("")
    protected val jidStore = new JIDStore { override val prefix = services.jid.toString }

    protected case class EchoAgentState(friends: Set[JID], status: Status)
    protected type State = EchoAgentState

    protected override def init = {
      val status = Status(<show>chat</show><status>Ready to echo</status>)
      EchoAgentState(Set() ++ jidStore.get, status)
    }

    protected override def message = super.message :+ echo

    protected def echo = mkMsg {
      case (Chat(_, thread, body, from),state) =>
        body match {
          case Command("status", text) =>
            log.info("Echo {} changing status to {}", name, text)
            atomic(_.copy(status=Status(<show>chat</show><status>{text}</status>)))
            announce
          case Command("offline", _) =>
            log.info("Echo {} goes offline", name)
            atomic(_.copy(status=Status(<status>Offline</status>, Some("unavailable"))))
            announce
          case Command("online", _) =>
            log.info("Echo {} comes back online", name)
            atomic(_.copy(status=Status(<show>chat</show><status>Ready to echo</status>)))
            announce
          case echo =>
            log.info("Echo {} echos message {}", name, body)
            services.send(Chat(None, thread, name+" says "+echo, from, services.jid)).receive
        }
    }
    protected object Command {
      def unapply(body: String) = {
        val (command, args) = body.span(_ != ' ')
        if (command.length > 0) Some(command.toLowerCase, args.drop(1))
        else None
      }
    }

    protected override def acceptSubscription(from: JID, content: NodeSeq)(state: State) = {
      val f = state.friends + from
      jidStore.set(f.toList)
      state.copy(friends=f)
    }
    protected override def removeSubscription(from: JID)(state: State) = {
      val f = state.friends.filterNot(_ == from)
      jidStore.set(f.toList)
      state.copy(friends=f)
    }
    protected override def status(state: State) = state.status
  }

  /**
   * Agent that shows information about the component
   */
  class AboutAgent(override protected val services: AgentServices, manager: AgentManager) extends PresenceManager {
    protected case class InfoAgentState(friends: Set[JID])
    protected type State = InfoAgentState

    protected override val stateless = new ComponentInfoAgent {
      override val services = AboutAgent.this.services
      override val manager = AboutAgent.this.manager
    }
    
    protected override def init = InfoAgentState(Set() ++ jidStore.get)
    protected val jidStore = new JIDStore { override val prefix = services.jid.toString }

    protected override def acceptSubscription(from: JID, content: NodeSeq)(state: State) = {
      val f = state.friends + from
      jidStore.set(f.toList)
      state.copy(friends=f)
    }
    protected override def removeSubscription(from: JID)(state: State) = {
      val f = state.friends.filterNot(_ == from)
      jidStore.set(f.toList)
      state.copy(friends=f)
    }
  }



  val spec = AgentComponent.specification("Echo", "Echos everything said the members", "echo2", Some("secret")) { am =>
    am.register("hans",  s => Spawner.start(new EchoAgent(s), SpawnAsRequiredChild))
    am.register("peter",  s => Spawner.start(new EchoAgent(s), SpawnAsRequiredChild))
    am.register("about", s => Spawner.start(new AboutAgent(s, am), SpawnAsRequiredChild))
  }
  server.register(spec)
}}
