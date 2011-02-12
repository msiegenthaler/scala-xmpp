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
trait ComponentDiscoveryAgent extends StatelessAgent with Log {
  protected[this] val manager: AgentManager

  protected override def iqGet = {
    super.iqGet :+ discovery
  }

  def discovery = mkIqGet {
    case get @ FirstElem(e @ ElemName("query", "http://jabber.org/protocol/disco#info")) =>
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
}

/**
 * Agent that answers text (chat-) messages about its AgentComponent.
 * Commands:
 *  - list
 *  - status
 *  - help
 */
trait ComponentInfoAgent extends StatelessAgent with Log {
  protected[this] val manager: AgentManager
  protected[this] val callback: AgentServices

  protected override def message = super.message :+ commands

  def commands = mkMsg {
    case Chat(subject, Command("list"), from) =>
      log.debug("Agent-listing requested by {}", from)
      val cs = manager.registeredComponents.receiveOption(5 s).getOrElse(Map())
      val parts = cs.map(c => Text(c._1.stringRepresentation))
      val body = parts.foldLeft(NodeSeq.Empty) { (l,c) =>
        l ++ c ++ <br/> //newline seperate the components
      }.drop(1)
      callback.send(Chat(subject, body, from, callback.jid))

    case Chat(subject, Command("status"), from) =>
      log.debug("Status requested by {}", from)
      val body = "Agent "+callback.jid+" for component "+callback.jid.domain+" (connected to "+callback.serverJid+")"
      callback.send(Chat(subject, Text(body), from, callback.jid))

    case Chat(subject, Command("help"), from) =>
      val body = <b>Available Commands</b>
        <ul>
          <li>list: Lists all registered agents</li>
          <li>status: Status of the component</li>
        </ul>;
      callback.send(Chat(subject, body, from, callback.jid))
  }

  protected object Command {
    def unapply(body: NodeSeq) = {
      Some(body.text.takeWhile(_ != ' ').toLowerCase)
    }
  }
}
