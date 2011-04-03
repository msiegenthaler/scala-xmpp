package ch.inventsoft.scalaxmpp

import org.scalatest._
import matchers._


class JIDSpec extends Spec with ShouldMatchers {
  describe("JID") {
    describe("domain-only") {
      it("should accept inventsoft.ch") {
        JID("inventsoft.ch").domain should be("inventsoft.ch")
      }
      it("should accept winv-adm") {
        JID("winv-adm").domain should be("winv-adm")
      }
      it("should have node and resource as None") {
        val i = JID("inventsoft.ch")
        i.resource should be(None)
      }
      it("should not accept domain with spaces") {
        try {
          JID("hans mueller")
          fail
        } catch {
          case a: IllegalArgumentException => //ok
        }
      }
      it("should parse inventsoft.ch") {
        val i = JID.parse("inventsoft.ch")
        i.domain should be("inventsoft.ch")
        i.node should be(None)
        i.resource should be(None)
      }
      it("should toString inventsoft.ch") {
        JID("inventsoft.ch").toString should be("inventsoft.ch")
      }
      it("should toString local-adm") {
        JID("local-adm").toString should be("local-adm")
      }
    }
    describe("node and domain") {
      it("should accept mario@inventsoft.ch") {
        val i = JID("mario", "inventsoft.ch")
        i.domain should be("inventsoft.ch")
        i.node.get should be("mario")
      }
      it("should accept mario.siegenthaler@inventsoft.ch") {
        val i = JID("mario.siegenthaler", "inventsoft.ch")
        i.domain should be("inventsoft.ch")
        i.node.get should be("mario.siegenthaler")
      }
      it("should have resource as None") {
        val i = JID("mario", "inventsoft.ch")
        i.resource should be(None)
      }
      it("should not accept mario siegenthaler") {
        try {
          JID("mario siegenthaler", "inventsoft.ch")
          fail
        } catch {
          case a: IllegalArgumentException => //ok
        }
      }
      it("should parse mario@inventsoft.ch") {
        val i = JID.parse("mario@inventsoft.ch")
        i.domain should be("inventsoft.ch")
        i.node.get should be("mario")
        i.resource should be(None)
      }
      it("should toString mario.siegenthaler@inventsoft.ch") {
        val i = JID("mario.siegenthaler", "inventsoft.ch")
        i.toString should be("mario.siegenthaler@inventsoft.ch")
      }
      it("should toString mario@local") {
        val i = JID("mario", "local")
        i.toString should be("mario@local")
      }
    }
    describe("node, domain & resource") {
      it("should accept mario@inventsoft.ch/home") {
        val i = JID("mario", "inventsoft.ch", "home")
        i.domain should be("inventsoft.ch")
        i.node.get should be("mario")
        i.resource.get should be("home")
      }
      it("should parse mario@inventsoft.ch/home") {
        val i = JID.parse("mario@inventsoft.ch/home")
        i.domain should be("inventsoft.ch")
        i.node.get should be("mario")
        i.resource.get should be("home")
      }
      it("should toString mario@inventsoft.ch/home") {
        val i = JID("mario", "inventsoft.ch", "home")
        i.toString should be("mario@inventsoft.ch/home")
      }
    }
    describe("compare") {
      it("should equal mario@inventsoft.ch") {
        val a = JID("mario", "inventsoft.ch")
        val b = JID("mario", "inventsoft.ch")
        a should be(b)
        a.hashCode should be(b.hashCode)
        a.compare(b) should be(0)
        b should be(a)
        b.compare(a) should be(0)
      }
      it("should equal mario@inventsoft.ch/home") {
        val a = JID("mario", "inventsoft.ch", "home")
        val b = JID("mario", "inventsoft.ch", "home")
        a should be(b)
        a.hashCode should be(b.hashCode)
        a.compare(b) should be(0)
        b should be(a)
        b.compare(a) should be(0)
      }
      it("should recognize john@mycompany.com as bigger than mycompany.com") {
        val a = JID.parse("john@mycompany.com")
        val b = JID.parse("mycompany.com")
        a.equals(b) should be(false)
        b.equals(a) should be(false)
        a.compareTo(b) should be > 0 
        b.compareTo(a) should be < 0 
      }
      it("should recognize john@mycompany.com as bigger than mycompany.com/resource") {
        val a = JID.parse("john@mycompany.com")
        val b = JID.parse("mycompany.com/resource")
        a.equals(b) should be(false)
        b.equals(a) should be(false)
        a.compareTo(b) should be > 0 
        b.compareTo(a) should be < 0 
      }
      it("should recognize john@mycompany.com as smaller than john@mycompany.com/resource") {
        val a = JID.parse("john@mycompany.com")
        val b = JID.parse("john@mycompany.com/resource")
        a.equals(b) should be(false)
        b.equals(a) should be(false)
        a.compareTo(b) should be < 0 
        b.compareTo(a) should be > 0 
      }
    }
    describe("isParentOf") {
      it("inventsoft.ch should be parent of inventsoft.ch") {
        assert(JID.parse("inventsoft.ch") isParentOf JID.parse("inventsoft.ch"))
      }
      it("ms@inventsoft.ch should be parent of ms@inventsoft.ch") {
        assert(JID.parse("ms@inventsoft.ch") isParentOf JID.parse("ms@inventsoft.ch"))
      }
      it("ms@inventsoft.ch/iPhone should be parent of ms@inventsoft.ch/iPhone") {
        assert(JID.parse("ms@inventsoft.ch/iPhone") isParentOf JID.parse("ms@inventsoft.ch/iPhone"))
      }
      it("ms@inventsoft.ch should be parent of ms@inventsoft.ch/iPhone") {
        assert(JID.parse("ms@inventsoft.ch") isParentOf JID.parse("ms@inventsoft.ch/iPhone"))
      }
      it("inventsoft.ch should be parent of ms@inventsoft.ch") {
        assert(JID.parse("inventsoft.ch") isParentOf JID.parse("ms@inventsoft.ch"))
      }
      it("inventsoft.ch should be parent of ms@inventsoft.ch/iPhone") {
        assert(JID.parse("inventsoft.ch") isParentOf JID.parse("ms@inventsoft.ch/iPhone"))
      }
      it("inventsoft.ch should not be parent of test.inventsoft.ch") {
        assert(!(JID.parse("inventsoft.ch") isParentOf JID.parse("test.inventsoft.ch")))
      }
      it("inventsoft.ch should not be parent of a@test.inventsoft.ch") {
        assert(!(JID.parse("inventsoft.ch") isParentOf JID.parse("a@test.inventsoft.ch")))
      }
      it("ms@inventsoft.ch should not be parent of msi@inventsoft.ch") {
        assert(!(JID.parse("ms@inventsoft.ch") isParentOf JID.parse("msi@inventsoft.ch")))
      }
      it("ms@inventsoft.ch should not be parent of msi@inventsoft.ch/XX") {
        assert(!(JID.parse("ms@inventsoft.ch") isParentOf JID.parse("msi@inventsoft.ch/XX")))
      }
      it("ms@inventsoft.ch/iPhone should not be parent of msi@inventsoft.ch/iPhone4") {
        assert(!(JID.parse("ms@inventsoft.ch/iPhone") isParentOf JID.parse("ms@inventsoft.ch/iPhone4")))
      }
      it("ms@inventsoft.ch/iPhone should not be parent of ms@inventsoft.ch") {
        assert(!(JID.parse("ms@inventsoft.ch/iPhone") isParentOf JID.parse("ms@inventsoft.ch")))
      }
      it("ms@inventsoft.ch/iPhone should not be parent of inventsoft.ch") {
        assert(!(JID.parse("ms@inventsoft.ch/iPhone") isParentOf JID.parse("inventsoft.ch")))
      }
      it("ms@inventsoft.ch should not be parent of inventsoft.ch") {
        assert(!(JID.parse("ms@inventsoft.ch") isParentOf JID.parse("inventsoft.ch")))
      }
    }
  }
}
