package ch.inventsoft
package scalaxmpp

import org.scalatest._
import matchers._
import scala.xml._


class XMPPPacketSpec extends Spec with ShouldMatchers {
  describe("IQ get") {
    it("should serialize") {
      val stanza = IQGet("123", user1_iPhone, user1, <query xmlns="jabber:iq:roster"/>)
      stanza.xml should be(
        <iq type="get" from="peter@xmpp.inventsoft.ch/iphone" to="peter@xmpp.inventsoft.ch" id="123"><query xmlns="jabber:iq:roster"/></iq>)
    }
    it("should parse") {
      val xml = <iq type="get" from="peter@xmpp.inventsoft.ch/iphone" to="peter@xmpp.inventsoft.ch" id="123"><query xmlns="jabber:iq:roster"/></iq>
      val stanza = XMPPPacket(xml)
      stanza match {
        case IQGet(id, from, to, content) =>
          from should be(user1_iPhone)
          to should be(user1)
          id should be("123")
          content(0) should be(<query xmlns="jabber:iq:roster"/>)
        case _ => fail("unexpected: "+stanza)
      }
    }
  }


  describe("IQ set") {
    it("should serialize") {
      val stanza = IQSet("123", user1_iPhone, user1, <query xmlns="jabber:iq:roster"><item jid={user2.stringRepresentation} /></query>)
      stanza.xml should be(
        <iq type="set" from="peter@xmpp.inventsoft.ch/iphone" to="peter@xmpp.inventsoft.ch" id="123"><query xmlns="jabber:iq:roster"><item jid="marc@xmpp.inventsoft.ch"/></query></iq>)
    }
    it("should parse") {
      val xml = <iq type="set" from="peter@xmpp.inventsoft.ch/iphone" to="peter@xmpp.inventsoft.ch" id="123"><query xmlns="jabber:iq:roster"><item jid="marc@xmpp.inventsoft.ch"/></query></iq>
      val stanza = XMPPPacket(xml)
      stanza match {
        case IQSet(id, from, to, content) =>
          from should be(user1_iPhone)
          to should be(user1)
          id should be("123")
          content(0) should be(<query xmlns="jabber:iq:roster"><item jid="marc@xmpp.inventsoft.ch"/></query>)
        case _ => fail("unexpected: "+stanza)
      }
    }
  }
  describe("IQ result") {
    it("should serialize") {
      val stanza = IQResult("1234", user1, user1_iPhone, <query xmlns="jabber:iq:roster"><item jid={user2.stringRepresentation}/></query>)
      stanza.xml should be(
        <iq type="result" from="peter@xmpp.inventsoft.ch" to="peter@xmpp.inventsoft.ch/iphone" id="1234"><query xmlns="jabber:iq:roster"><item jid="marc@xmpp.inventsoft.ch"/></query></iq>)
    }
    it("should parse") {
      val xml = <iq type="result" from="peter@xmpp.inventsoft.ch" to="peter@xmpp.inventsoft.ch/iphone" id="1234"><query xmlns="jabber:iq:roster"><item jid="marc@xmpp.inventsoft.ch"/></query></iq>
      val stanza = XMPPPacket(xml)
      stanza match {
        case IQResult(id, from, to, content) =>
          from should be(user1)
          to should be(user1_iPhone)
          id should be("1234")
          content(0) should be(<query xmlns="jabber:iq:roster"><item jid={user2.stringRepresentation}/></query>)
        case _ => fail("unexpected: "+stanza)
      }
    }
  }
  describe("IQ error") {
    it("should parse") {
      val xml = <iq type="error" from="romeo@example.net" to="tybalt@example.com/pda" id="probing1"><query xmlns="jabber:iq:version"/><error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error></iq>
      val p = XMPPPacket(xml)
      p match {
        case p: IQError =>
          p.id should be("probing1")
          p.from should be(JID("romeo", "example.net"))
          p.to should be(JID("tybalt", "example.com", "pda"))
          p.errorType should be("cancel")
          p.errorText should be(None)
          p.errorCondition should be(Some(<service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/>))
          n(p.otherContent) should be(n(<query xmlns="jabber:iq:version"></query>))
        case other => fail(""+other)
      }
    }
    it("should serialize") {
      val stanza = IQError("probing1", JID("romeo", "example.net"), JID("tybalt", "example.com", "pda"), <error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>, <query xmlns="jabber:iq:version"/>)
      stanza.xml should be(<iq type="error" from="romeo@example.net" to="tybalt@example.com/pda" id="probing1"><query xmlns="jabber:iq:version"/><error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error></iq>)
    }
    it("should serialize and reparse") {
      val stanza = IQError("probing1", JID("romeo", "example.net"), JID("tybalt", "example.com", "pda"), <error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>, <query xmlns="jabber:iq:version"/>)
      val p=XMPPPacket(stanza.xml)
      p match {
        case p: IQError =>
          p.id should be("probing1")
          p.from should be(JID("romeo", "example.net"))
          p.to should be(JID("tybalt", "example.com", "pda"))
          p.errorType should be("cancel")
          p.errorText should be(None)
          p.errorCondition should be(Some(<service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/>))
          n(p.otherContent) should be(n(<query xmlns="jabber:iq:version"></query>))
        case other => fail(""+other)
      }
    }
  }

  describe("Message") {
    it("should serialize without id") {
      val stanza = MessageSend(None, "chat", user1_iPhone, user2, <body>Who are you?</body><subject>Query</subject>)
      stanza.xml should be(<message type="chat" from="peter@xmpp.inventsoft.ch/iphone" to="marc@xmpp.inventsoft.ch"><body>Who are you?</body><subject>Query</subject></message>)
    }
    it("should parse without id") {
      val xml = <message type="chat" from="peter@xmpp.inventsoft.ch/iphone" to="marc@xmpp.inventsoft.ch"><body>Who are you?</body><subject>Query</subject></message>
      val stanza = XMPPPacket(xml)
      stanza match {
        case MessageSend(id, t, from, to, content) =>
          id should be(None)
          t should be("chat")
          from should be(user1_iPhone)
          to should be(user2)
          content(0) should be(<body>Who are you?</body>)
          content(1) should be(<subject>Query</subject>)
        case _ => fail("unexpected: "+stanza)
      }
    }
    it("should serialize with id") {
      val stanza = MessageSend(Some("1234"), "chat", user1_iPhone, user2, <body>Who are you?</body><subject>Query</subject>)
      n(stanza.xml) should be(n(<message type="chat" from="peter@xmpp.inventsoft.ch/iphone" to="marc@xmpp.inventsoft.ch" id="1234"><body>Who are you?</body><subject>Query</subject></message>))
    }
    it("should parse with id") {
      val xml = <message id="1234" type="chat" from="peter@xmpp.inventsoft.ch/iphone" to="marc@xmpp.inventsoft.ch"><body>Who are you?</body><subject>Query</subject></message>
      val stanza = XMPPPacket(xml)
      stanza match {
        case MessageSend(id, t, from, to, content) =>
          id should be(Some("1234"))
          t should be("chat")
          from should be(user1_iPhone)
          to should be(user2)
          content(0) should be(<body>Who are you?</body>)
          content(1) should be(<subject>Query</subject>)
        case _ => fail("unexpected: "+stanza)
      }
    }
    it("should allow for error without additional content") {
      val e = MessageError(None, JID.parse("hans@meier.com"), JID.parse("peter@mueller.com"), <error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>)
      e.xml should be(<message from="hans@meier.com" to="peter@mueller.com" type="error"><error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error></message>)
    }
    it("should allow for error with additional content") {
      val e = MessageError(None, JID.parse("hans@meier.com"), JID.parse("peter@mueller.com"), <error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>, <subject>Hello world</subject><body>Hi there!</body>)
      e.xml should be(<message from="hans@meier.com" to="peter@mueller.com" type="error"><subject>Hello world</subject><body>Hi there!</body><error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error></message>)
    }
    it("should parse errors without additional content") {
      val xml = <message from="hans@meier.com" to="peter@mueller.com" type="error"><error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error></message>
      val stanza = XMPPPacket(xml)
      stanza should be(MessageError(None, JID.parse("hans@meier.com"), JID.parse("peter@mueller.com"), <error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>))
    }
    it("should parse errors with additional content") {
      val xml = <message from="hans@meier.com" to="peter@mueller.com" type="error"><subject>Hello world</subject><body>Hi there!</body><error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error></message>
      val stanza = XMPPPacket(xml)
      stanza should be(MessageError(None, JID.parse("hans@meier.com"), JID.parse("peter@mueller.com"), <error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>, <subject>Hello world</subject><body>Hi there!</body>))
    }
  }

  describe("Presence") {
    it("should serialize without id, type and to") {
      val stanza = Presence(user1_iPhone, <show>xa</show><status>Having fun</status>)
      stanza.xml should be(<presence from="peter@xmpp.inventsoft.ch/iphone"><show>xa</show><status>Having fun</status></presence>)
    }
    it("should parse without id, type and to") {
      val xml = <presence from="peter@xmpp.inventsoft.ch/iphone"><show>xa</show><status>Having fun</status></presence>
      val stanza = XMPPPacket(xml)
      stanza match {
        case Presence(from, content, t, to, id) =>
          from should be(user1_iPhone)
          content(0) should be(<show>xa</show>)
          content(1) should be(<status>Having fun</status>)
          t should be(None)
          to should be(None)
          id should be(None)
        case _ => fail("Unexepected "+stanza)
      }
    }
    it("should serialize with id, type and to") {
      val stanza = Presence(user1_iPhone, <show>xa</show><status>Having fun</status>, Some("unavailable"), Some(user2), Some("1234"))
      n(stanza.xml) should be(n(<presence to="marc@xmpp.inventsoft.ch" from="peter@xmpp.inventsoft.ch/iphone" id="1234" type="unavailable"><show>xa</show><status>Having fun</status></presence>))
    }
    it("should parse with id, type and to") {
      val xml = <presence to="marc@xmpp.inventsoft.ch" from="peter@xmpp.inventsoft.ch/iphone" id="1234" type="unavailable"><show>xa</show><status>Having fun</status></presence>
      val stanza = XMPPPacket(xml)
      stanza match {
        case Presence(from, content, t, to, id) =>
          from should be(user1_iPhone)
          content(0) should be(<show>xa</show>)
          content(1) should be(<status>Having fun</status>)
          t should be(Some("unavailable"))
          to should be(Some(user2))
          id should be(Some("1234"))
        case other => fail(other.toString)
      }
    }
    it("should serialize an error") {
      val stanza = PresenceError(JID("mercutio","example.org"), <error type="cancel"><gone xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>, None, Some(JID("romeo", "example.net", "orchard")))
      val p = XMPPPacket(stanza.xml)
      stanza should be(p)
    }
    it("should parse an error") {
      val xml = <presence type="error" from="mercutio@example.org" to="romeo@example.net/orchard"><error type="cancel"><gone xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error></presence>
      val stanza = XMPPPacket(xml)
      stanza should be(PresenceError(JID("mercutio","example.org"), <error type="cancel"><gone xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>, None, Some(JID("romeo", "example.net", "orchard"))))
    }
  }

  describe("stanza error") {
    val em1 = <message from="peter@xmpp.inventsoft.ch/iphone" to="marc@xmpp.inventsoft.ch" type="error">
                <error type="cancel">
                  <service-unavailable xmlns="urn:etf:params:xml-ns=xmppp-stanzas"/>
                </error>
              </message>

    it("should parse an error") {
      val stanza = XMPPPacket(em1)
      stanza match {
        case e: MessageError =>
          e.errorType should be("cancel")
          e.errorText should be(None)
          e.errorCondition should be(Some(<service-unavailable xmlns="urn:etf:params:xml-ns=xmppp-stanzas"/>))
          e.errorElem.label should be("error")
          e.errorElem.attribute("type") should be(Some(Text("cancel")))
        case _ => fail("unexpected: "+stanza)
      }
    }
    it("should allow constructing an error") {
      val e = MessageError(None, JID.parse("hans@meier.com"), JID.parse("peter@mueller.com"), <error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>)
      e.xml should be(<message from="hans@meier.com" to="peter@mueller.com" type="error"><error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error></message>)
    }
  }

  describe("stream error") {
    it("should parse") {
      val xml = <stream:error><conflict xmlns="urn:ietf:params:xml:ns:xmpp-streams"/></stream:error>
      val stanza = XMPPPacket(xml)
      stanza match {
        case stanza: StreamError =>
          n(stanza.content) should be(n(<conflict xmlns="urn:ietf:params:xml:ns:xmpp-streams"/>))
        case other => fail(other.toString)
      }
    }
    it("should serialize") {
      val stanza = StreamError(<conflict xmlns="urn:ietf:params:xml:ns:xmpp-streams"/>)
      stanza.xml should be(<stream:error><conflict xmlns="urn:ietf:params:xml:ns:xmpp-streams"/></stream:error>)
    }
  }
  
  def n(ns: NodeSeq): NodeSeq = {
    XML.loadString(ns.toString)
  }

  val user1 = JID("peter", "xmpp.inventsoft.ch")
  val user1_iPhone = JID("peter", "xmpp.inventsoft.ch", "iphone")
  val user2 = JID("marc", "xmpp.inventsoft.ch")
}
