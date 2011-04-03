package ch.inventsoft
package scalaxmpp


/**
 * A Jabber identifier (JID) is used to an XMPP endpoint. See http://xmpp.org/rfcs/rfc3920.html
 * for more information.<br>
 * Definition:
 *   jid             = [ node "@" ] domain [ "/" resource ]
 *   domain          = fqdn / address-literal
 *   fqdn            = (sub-domain 1*("." sub-domain))
 *   sub-domain      = (internationalized domain label)
 *   address-literal = IPv4address / IPv6address
 */
trait JID extends Ordered[JID] {
  val node: Option[String]
  val domain: String
  val resource: Option[String]
  def isParentOf(other: JID) = {
    domain == other.domain && 
      (if (node.isDefined) node == other.node else true) && 
      (if (resource.isDefined) resource==other.resource else true)
  }
  def withoutResource: JID = {
    if (resource.isDefined) {
      JID(node, domain, None)
    } else this
  }
  def stringRepresentation = {
    node.map(_+"@").getOrElse("") + domain + resource.map("/"+_).getOrElse("")
  }
  override def toString = stringRepresentation
  override def hashCode = domain.hashCode ^ node.hashCode ^ resource.hashCode
  override def equals(other: Any) = other match {
    case that: JID =>
      domain==that.domain && node==that.node && resource==that.resource
    case other => false
  }
  override def compare(that: JID) = {
    def cmp(a: Option[String], b: Option[String]): Int = a match {
      case Some(a) => if (b.isDefined) a.compareTo(b.get) else 1
      case None => if (b.isDefined) -1 else 0
    }
    // domain, node, resource
    val d = domain.compare(that.domain)
    if (d == 0) {
      val n = cmp(node, that.node)
      if (n == 0) {
        cmp(resource, that.resource)
      } else n
    } else d
  }
}

object JID {
  def parseOption(stringRepresentation: String) = {
    try {
      val jid = parse(stringRepresentation)
      Some(jid)
    } catch {
      case e: Exception => None
    }
  }
  def parse(stringRepresentation: String): JID = {
    val internal = new org.xmpp.packet.JID(stringRepresentation)
    new OrgXmppWrapper(internal)
  }
  def apply(domain: String): JID = apply(None, domain, None)
  def apply(node: String, domain: String): JID = apply(Some(node), domain, None)
  def apply(node: String, domain: String, resource: String): JID = this(Some(node), domain, Some(resource))
  def apply(node: Option[String], domain: String, resource: Option[String]): JID = {
    val internal = new org.xmpp.packet.JID(node.getOrElse(null), domain, resource.getOrElse(null))
    new OrgXmppWrapper(internal)
  }
  def unapply(jid: JID): Option[(Option[String],String,Option[String])] = {
    Some((jid.node, jid.domain, jid.resource))
  }

  private class OrgXmppWrapper(val internal: org.xmpp.packet.JID) extends JID {
    override val node = nullToNone(internal.getNode)
    override val domain = internal.getDomain
    override val resource = nullToNone(internal.getResource)
    override def stringRepresentation = internal.toString
    private def nullToNone[A](value: A) = {
      if (value != null) Some(value)
      else None
    }
  }
}
