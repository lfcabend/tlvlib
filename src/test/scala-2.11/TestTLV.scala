/**
 * Created by lau on 15-7-15.
 */

import org.scalatest._
import HexUtils._
import TLV._

import scala.collection.immutable.Vector

class TestTLV extends FlatSpec with Matchers {

  "A BerTag" should "be able to be constructed" in {
    val t = BerTag("80")
    t.value should be(hex2Bytes("80"))
  }

  it should "set the constructed flag to false for non constructed tags" in {
    val t = BerTag("80")
    t.isConstructed should be (false)
  }

  it should "set the constructed flag to true for constructed tags" in {
    val t = BerTag("70")
    t.isConstructed should be (true)
  }

  it should "be able to have two bytes" in {
    val t = BerTag("1F00")
    t.value should be(hex2Bytes("1F00"))
  }

  it should "be able to have tree or more bytes" in {
    val t = BerTag("1F8000")
    t.value should be(hex2Bytes("1F8000"))
  }

  it should "fail to construct when two bytes are passed, but first byte does not indicate more bytes" in {
    intercept[IllegalArgumentException] {
      BerTag("1000")
    }
  }

  it should "fail to construct null is passed as the value" in {
    intercept[IllegalArgumentException] {
      BerTag(null)
    }
  }

  it should "fail to construct when three bytes are passed, but second byte does not indicate more bytes" in {
    intercept[IllegalArgumentException] {
      BerTag("1F0000")
    }
  }

  "A BerTLVLeaf" should "be able to be constructed" in {
    val v = BerTLVLeaf(BerTag("80"), "0000")
    v.tag should be(BerTag("80"))
    v.tag.value should be(hex2Bytes("80"))
    v.length should be(2)
    v.value should be(hex2Bytes("0000"))
  }

  it should "also be able to construct using the operator" in {
    val v = "80" >>: "0000"
    v.tag should be(BerTag("80"))
    v.tag.value should be(hex2Bytes("80"))
    v.length should be(2)
    v.value should be(hex2Bytes("0000"))
  }

  it should "be able to have no value" in {
    val v = BerTLVLeaf(BerTag(hex2Bytes("80")), Nil)
    v.length should be(0)
    v.value should be(Nil)
  }

  it should "be possible to serialize to bytes" in {
    val v = BerTLVLeaf(BerTag(hex2Bytes("80")), hex2Bytes("0000"))
    v.serializeTLV should be(hex2Bytes("80020000"))
  }

}
