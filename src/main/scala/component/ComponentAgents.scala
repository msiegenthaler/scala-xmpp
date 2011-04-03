package ch.inventsoft
package scalaxmpp
package component

import scala.xml._
import scalabase.process._
import scalabase.time._
import scalabase.oip._
import scalabase.log._
import Messages._


/**
 * Agent that answers discovery-IQs about its AgentComponent.
 */
abstract class ComponentDiscoveryAgent extends StateServerAgent with HandlerAgent with Log {
  protected val manager: AgentManager

  protected override def iqGet = super.iqGet :+ discovery

  def discovery = mkIqGet {
    case (get @ FirstElem(ElemName("query", "http://jabber.org/protocol/disco#info")),state) =>
      log.debug("Discovery request from {}", get.from)
      val agents = manager.registeredComponents.receiveOption(5 s).getOrElse(Map());
      noop
      get.resultOk {
        <query xmlns="http://jabber.org/protocol/disco#items">
        {agents.map(_._1).map { agent =>
          <item jid="{agent.stringRepresentation}"/>
        }}
        </query>
      }
  }
  
  override def toString = "ComponentDiscoveryAgent"
}

/**
 * Agent that answers text (chat-) messages about its AgentComponent.
 * Commands:
 *  - list
 *  - status
 *  - help
 */
abstract class ComponentInfoAgent extends StateServerAgent with HandlerAgent with Log {
  protected val manager: AgentManager
  protected val services: AgentServices

  protected override def message = super.message :+ commands

  def commands = mkMsg {
    case Chat(_, thread, Command("list"), from) =>
      log.debug("Agent-listing requested by {}", from)
      val cs = manager.registeredComponents.receiveOption(5 s).getOrElse(Map())
      val parts = cs.map(c => Text(c._1.stringRepresentation))
      val body = parts.mkString("\n")
      services.send(Chat(None, thread, body, from, services.jid)).receive

    case Chat(_, thread, Command("status"), from) =>
      log.debug("Status requested by {}", from)
      val body = "Agent "+services.jid+" for component "+services.jid.domain+" (connected to "+services.serverJid+")"
      services.send(Chat(None, thread, body, from, services.jid)).receive

    case Chat(_, thread, Command("help"), from) =>
      val commands = ("list", "Lists all registered agents") ::
                     ("status", "Status of the component") ::
                     ("help", "Shows this output") :: Nil
      val body = "Available Commands:\n"+commands.map(c => " "+c._1+": "+c._2).mkString("\n")
      services.send(Chat(None, thread, body, from, services.jid)).receive
  }

  protected object Command {
    def unapply(body: String) = {
      Some(body.takeWhile(_ != ' ').toLowerCase)
    }
  }

  override def toString = "ComponentInfoAgent"
}
