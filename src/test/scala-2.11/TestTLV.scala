/**
 * Created by lau on 15-7-15.
 */

import org.scalatest._
import HexUtils._
import TLV._

import scala.collection.immutable.Vector

class TestTLV extends FlatSpec with Matchers {

  "A BerTLVLeaf" should "be able to be constructed" in {
    val v = BerTLVLeaf(BerTag("80"), "0000")
    val tag = BerTag("80")
    v.tag should be(tag)
    v.tag.value should be(hex2Bytes("80"))
  }

}
