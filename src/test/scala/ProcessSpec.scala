package ch.inventsoft
package scalaxmpp

import org.scalatest._
import matchers._
import scalabase.process._
import scalabase.time._


trait ProcessSpec extends Spec {
  protected[this] def it_(name: String)(body: => Unit @process): Unit = it(name) {
    spawnAndBlock(body)
  }
  
  import ShouldMatchers._
  protected[this] def assertEquals(a: Any, b: Any) = {
    a should be(b)
  }
  
  protected[this] def sleep(forTime: Duration) = {
    receiveWithin(forTime) { case Timeout => () }
  }
}
