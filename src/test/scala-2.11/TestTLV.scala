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
    t.isConstructed should be(false)
  }

  it should "set the constructed flag to true for constructed tags" in {
    val t = BerTag("70")
    t.isConstructed should be(true)
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

  it should "serialize a length at 127 correctly on 1 byte" in {
    val bytes: Seq[Byte] = List.fill(127) {
      0.toByte
    }
    val v = BerTLVLeaf(BerTag(hex2Bytes("80")), bytes)
    v.length should be(127)
    v.serializeTLV should be(hex2Bytes("807F") ++ bytes)
  }

  it should "serialize a length at 128 correctly on 2 bytes" in {
    val bytes: Seq[Byte] = List.fill(128) {
      0.toByte
    }
    val v = BerTLVLeaf(BerTag(hex2Bytes("80")), bytes)
    v.length should be(128)
    v.serializeTLV should be(hex2Bytes("808180") ++ bytes)
  }

  it should "serialize a length at 255 correctly on 2 bytes" in {
    val bytes: Seq[Byte] = List.fill(255) {
      0.toByte
    }
    val v = BerTLVLeaf(BerTag(hex2Bytes("80")), bytes)
    v.length should be(255)
    v.serializeTLV should be(hex2Bytes("8081FF") ++ bytes)
  }

  it should "serialize a length at 256 correctly on 3 bytes" in {
    val bytes: Seq[Byte] = List.fill(256) {
      0.toByte
    }
    val v = BerTLVLeaf(BerTag(hex2Bytes("80")), bytes)
    v.length should be(256)
    v.serializeTLV should be(hex2Bytes("80820100") ++ bytes)
  }

  it should "be possible to be create with a length equal to the int maximum value" in {
    val bytes = new Array[Byte](Int.MaxValue)
    val v = BerTLVLeaf(BerTag(hex2Bytes("80")), bytes)
    v.length should be(0x7FFFFFFF)
    v.value should be(bytes)
  }

  it should "support niels's testcase constructor 1" in {
    val leaf = BerTLVLeaf(BerTag(hex2Bytes("8C")), hex2Bytes("0102030405060708"))
    leaf.serializeTLV should be(hex2Bytes("8C080102030405060708"))
    leaf.tag.value should be(hex2Bytes("8C"))
    leaf.length should be(8)
  }

  ignore should "be possible to serialize with int maximum length value" in {
    val bytes = new Array[Byte](Int.MaxValue)
    val v = BerTLVLeaf(BerTag(hex2Bytes("80")), bytes)
    v.length should be(0x7FFFFFFF)
    v.serializeTLV //this seems to have a weird value
    //probably due to the fact that the length becomes larger than the max int value, with the tag and
    //length serialized
  }

  ignore should "not be able to be create with a value with a length larger then Int.MaxValue" in {
    intercept[IllegalArgumentException] {
      //todo
    }
  }


  "A BerTLVCons" should "be able to be constructed" in {
    val v0 = BerTLVLeaf(BerTag("80"), "0000")

    val v = BerTLVCons(BerTag("A5"), List(v0))

    v.tag should be(BerTag("A5"))
    v.constructedValue should be(List(v0))
    v.value should be(hex2Bytes("80020000"))
  }

  it should "be possible to have nested BerTLVCons" in {
    val v0 = BerTLVLeaf(BerTag("80"), "0000")
    val v1 = BerTLVCons(BerTag("A5"), List(v0))
    val v2 = BerTLVCons(BerTag("70"), List(v1))

    v2.tag should be(BerTag("70"))
    v2.constructedValue should be(List(v1))
    v2.value should be(hex2Bytes("A50480020000"))
  }

  it should "be possible to serialize" in {
    val v0 = BerTLVLeaf(BerTag("80"), "0000")
    val v1 = BerTLVCons(BerTag("A5"), List(v0))
    val v2 = BerTLVCons(BerTag("70"), List(v1))

    v2.serializeTLV should be(hex2Bytes("7006A50480020000"))
  }

  it should "be possible to create without a value" in {
    val v = BerTLVCons(BerTag("A5"), List())
    v.value should be(Nil)
    v.constructedValue should be(Nil)
  }

  it should "be possible to create with multiple values" in {
    val v0 = BerTLVLeaf(BerTag("80"), "0000")
    val v = BerTLVCons(BerTag("A5"), List(v0, v0, v0, v0, v0))
    v.value should be(hex2Bytes("8002000080020000800200008002000080020000"))
    v.length should be(20)
    v.constructedValue should be(List(v0, v0, v0, v0, v0))
  }

  it should "be possible to create with multiple nested values" in {
    val v0 = BerTLVLeaf(BerTag("80"), "0000")
    val v1 = BerTLVCons(BerTag("A5"), List(v0, v0))
    val v2 = BerTLVCons(BerTag("70"), List(v1, v1, v1))

    v2.value should be(hex2Bytes("A5088002000080020000A5088002000080020000A5088002000080020000"))
    v2.constructedValue should be(List(v1, v1, v1))

    val validate80Tag:(BerTLV => Unit) = {
      case y @ BerTLVLeaf(t, cv) => {
        t should be(BerTag("80"))
        y.length should be(2)
        cv should be(hex2Bytes("0000"))
      }
      case _ => fail("Should have been a BerTLVCons")
    }

    val validateA5Tag: (BerTLV => Unit) = {
      case y @ BerTLVCons(t, cv) => {
        t should be(BerTag("A5"))
        y.length should be(8)
        cv should be(List(v0, v0))
        cv.map(validate80Tag)
      }
      case _ => fail("Should have been a BerTLVCons")
    }


    v2.constructedValue.map(validateA5Tag)
  }

  it should "be possible to visit each value with for each" in {
    val v01 = BerTLVLeaf(BerTag("80"), "0000")
    val v02 = BerTLVLeaf(BerTag("81"), "0001")
    val v11 = BerTLVCons(BerTag("A5"), List(v01, v02))
    val v12 = BerTLVCons(BerTag("A6"), List(v02, v01))
    val v2 = BerTLVCons(BerTag("70"), List(v11, v12))
    var mutableString = ""
    v2.foreach(x => mutableString += x.tag().toString())
    mutableString should be("70A58081A68180")
  }

  it should "be possible to filter" in {
    val v01 = BerTLVLeaf(BerTag("80"), "0000")
    val v02 = BerTLVLeaf(BerTag("81"), "0001")
    val v11 = BerTLVCons(BerTag("A5"), List(v01, v02))
    val v12 = BerTLVCons(BerTag("A6"), List(v02, v01))
    val v2 = BerTLVCons(BerTag("70"), List(v11, v12))

    v2.filter(x => x.tag == v01.tag || x.tag == v02.tag) should be(List(v01, v02, v02, v01))
  }


  "A TLV parser" should "be able to parse a BerTLVLeaf with 1 byte tag and 1 byte length" in {
    val parser = new TLVParsers()

    val leaf = parser.parseTLV("80020000").get
    leaf.tag() should be(BerTag("80"))
    leaf.length should be(2)
    leaf.value should be(hex2Bytes("0000"))
  }

  it should "be able to parse a BerTLVLeaf with no value" in {
    val parser = new TLVParsers()

    val leaf = parser.parseTLV("8000").get
    leaf.tag() should be(BerTag("80"))
    leaf.length should be(0)
    leaf.value should be(Nil)
  }

  it should "be able to parse a BerTLVLeaf with 2 byte tag" in {
    val parser = new TLVParsers()

    val leaf = parser.parseTLV("9F01020001").get
    leaf.tag() should be(BerTag("9F01"))
    leaf.length should be(2)
    leaf.value should be(hex2Bytes("0001"))
  }

  it should "be able to parse a BerTLVLeaf with 3 byte tag" in {
    val parser = new TLVParsers()

    val leaf = parser.parseTLV("9F8101020001").get
    leaf.tag() should be(BerTag("9F8101"))
    leaf.length should be(2)
    leaf.value should be(hex2Bytes("0001"))
  }

  it should "be able to parse a BerTLVLeaf with 1 byte length value 127" in {
    val parser = new TLVParsers()

    val bytes: Seq[Byte] = List.fill(127) {
      0.toByte
    }

    val leaf = parser.parseTLV(hex2Bytes("807F") ++ bytes).get
    leaf.tag() should be(BerTag("80"))
    leaf.length should be(127)
    leaf.value should be(bytes)
  }

  it should "be able to parse a BerTLVLeaf with 2 byte length value 128" in {
    val parser = new TLVParsers()

    val bytes: Seq[Byte] = List.fill(128) {
      0.toByte
    }

    val leaf = parser.parseTLV(hex2Bytes("808180") ++ bytes).get
    leaf.tag() should be(BerTag("80"))
    leaf.length should be(128)
    leaf.value should be(bytes)
  }

  it should "be able to parse a BerTLVLeaf with 2 byte length value 255" in {
    val parser = new TLVParsers()

    val bytes: Seq[Byte] = List.fill(255) {
      0.toByte
    }

    val leaf = parser.parseTLV(hex2Bytes("8081FF") ++ bytes).get
    leaf.tag() should be(BerTag("80"))
    leaf.length should be(0xFF)
    leaf.value should be(bytes)
  }

  it should "be able to parse a BerTLVLeaf with 3 byte length value 256" in {
    val parser = new TLVParsers()

    val bytes: Seq[Byte] = List.fill(256) {
      0.toByte
    }

    val leaf = parser.parseTLV(hex2Bytes("8082010000") ++ bytes).get
    leaf.tag() should be(BerTag("80"))
    leaf.length should be(256)
    leaf.value should be(bytes)
  }

  it should "be able to parse a BerTLVCons with one nested Leaf" in {
    val parser = new TLVParsers()
    val v01 = BerTLVLeaf(BerTag("80"), "0001")
    parser.parseTLV("A50480020001").get match {
      case a @ BerTLVCons(t, c) => {
        t should be(BerTag("A5"))
        a.length should be(4)
        c.length should be(1)
        c(0) should be(v01)
      }
      case _ => fail("Should be a ber tlv cons")
    }
  }

  it should "be able to parse a BerTLVCons with two nested Leafs" in {
    val parser = new TLVParsers()
    val v01 = BerTLVLeaf(BerTag("80"), "0001")
    val v02 = BerTLVLeaf(BerTag("80"), "0002")
    parser.parseTLV("A5088002000180020002").get match {
      case a @ BerTLVCons(t, c) => {
        t should be(BerTag("A5"))
        a.length should be(8)
        c.length should be(2)
        c(0) should be(v01)
        c(1) should be(v02)
      }
      case _ => fail("Should be a ber tlv cons")
    }
  }

  it should "be able to parse a BerTLVCons with no value" in {
    val parser = new TLVParsers()
    parser.parseTLV("A500").get match {
      case a @ BerTLVCons(t, c) => {
        t should be(BerTag("A5"))
        a.length should be(0)
        c should be(Nil)
      }
      case _ => fail("Should be a ber tlv cons")
    }
  }

  it should "be able to parse a BerTLVCons with nested Cons" in {
    val parser = new TLVParsers()
    val v0 = BerTLVLeaf(BerTag("80"), "0000")
    val v1 = BerTLVCons(BerTag("A5"), List(v0, v0))
    val v2 = BerTLVCons(BerTag("70"), List(v1, v1, v1))

    parser.parseTLV("701EA5088002000080020000A5088002000080020000A5088002000080020000").get match {
      case a @ BerTLVCons(t, c) => {
        t should be(BerTag("70"))
        a.length should be(30)
        c.length should be(3)
        c(0) should be(v1)
        c(1) should be(v1)
        c(2) should be(v1)
      }
      case _ => fail("Should be a ber tlv cons")
    }
  }

  it should "be able to parse a public key in ASN 1 encoding" in {
    val publicKeyStr = "3081FF0281F900C1429F97688620FA3F6DA0E8C1669D4231B9AA3F86434E494F5D325CB2221E594378C19BF14EBB0F988295FCAE8BD516036E8F2A3C64CB2B95E467ED1EEFFEB99A1C2754ECED5A46CBDD96EF25345B5EF6C5558D9B384F292BB0E67607E7560F06C4367E4CD9CBCB72F40AD534139326EE62DC6D055B1C9D67979190A9618B88947174C046A0219D468F5B6ABC8A4CF92F56EA6897C4904947947156733AE5B6B79F3B4F9A71106638806CF37171E2F36BB684F7097541C8F38D51A61E4BE745653F23C647D3B6B9A95E02BD692EF34D6669D2431074BDF1E9B6D26104B76E7879F6E2E1CDD042312ADF8E25AC49739D6A4BBBA72A964E3B020103"
    val parser = new TLVParsers()
    val publicKeyValue = parser.parseTLV(publicKeyStr).get match {
      case a@BerTLVCons(t,v) => v
      case _ => fail("should contain cons")
    }
    publicKeyValue.length should be(2)
    publicKeyValue(0).tag.value should be(hex2Bytes("02"))
    publicKeyValue(0).length should be(0xF9)
    publicKeyValue(0).value should be(hex2Bytes("00C1429F97688620FA3F6DA0E8C1669D4231B9AA3F86434E494F5D325CB2221E594378C19BF14EBB0F988295FCAE8BD516036E8F2A3C64CB2B95E467ED1EEFFEB99A1C2754ECED5A46CBDD96EF25345B5EF6C5558D9B384F292BB0E67607E7560F06C4367E4CD9CBCB72F40AD534139326EE62DC6D055B1C9D67979190A9618B88947174C046A0219D468F5B6ABC8A4CF92F56EA6897C4904947947156733AE5B6B79F3B4F9A71106638806CF37171E2F36BB684F7097541C8F38D51A61E4BE745653F23C647D3B6B9A95E02BD692EF34D6669D2431074BDF1E9B6D26104B76E7879F6E2E1CDD042312ADF8E25AC49739D6A4BBBA72A964E3B"))
    publicKeyValue(1).tag.value should be(hex2Bytes("02"))
    publicKeyValue(1).length should be(1)
    publicKeyValue(1).value should be(hex2Bytes("03"))
  }

  it should "be able to parse a tlv list" in {
    val input = "4F07A0000000041010500A4D617374657243617264820278008701018C219F02069F03069F1A0295055F2A029A039C019F37049F35019F45029F4C089F34038D0C910A8A0295059F37049F4C088E1200000000000000004203440341035E031F039404080106015F25031002015F280206345F2D02656E5F3401029F0702FF009F080200029F0D05F8500400009F0E0500008800009F0F05F8700498009F1101019F120A4D6173746572436172649F1401009F2301009F420206349F4401029F450200009F49039F37049F4A0182C403405000C5030CF800C303000000CA06000000000000CB06000000000000C8020634C9020634D11906340000000634000000063400000006340000000634000000D5020C02D6020012DFFFFF08084F08878130ABEF48DFFFFF07742542313233343536303030303030303031375E53434D2054455354494E4720202020202020202020202020205E323031323230313137383739303030303030303030303433303030303030303F3B313233343536303030303030303031373D32303132323031313738373934333030303030303F"
    val parser = new TLVParsers()
    val r = parser.parseTLVList(input).get match {
      case v : List[BerTLV] => v
      case _ => fail("Should result in a list of TLV: ")
    }
    r.length should be(38)
    r(0) should be(BerTLVLeaf(BerTag("4F"), "A0000000041010"))
    r(37) should be(BerTLVLeaf(BerTag("DFFFFF07"), "2542313233343536303030303030303031375E53434D2054455354494E4720202020202020202020202020205E323031323230313137383739303030303030303030303433303030303030303F3B313233343536303030303030303031373D32303132323031313738373934333030303030303F"))
  }

  it should "be able to parse a tlv list with nested TLVs" in {
    val v0 = BerTLVLeaf(BerTag("80"), "0000")
    val v1 = BerTLVCons(BerTag("A5"), List(v0, v0))
    val v2 = BerTLVCons(BerTag("70"), List(v1, v1, v1))

    val input = "701EA5088002000080020000A5088002000080020000A5088002000080020000701EA5088002000080020000A5088002000080020000A5088002000080020000"
    val parser = new TLVParsers()
    val r = parser.parseTLVList(input).get match {
      case v : List[BerTLV] => v
      case _ => fail("Should result in a list of TLV: ")
    }
    r.length should be(2)
    r(0) should be (v2)
    r(1) should be (v2)
  }

  it should "be able to parse a tlv list with TLVCons and Leafs" in {
    val v0 = BerTLVLeaf(BerTag("80"), "0000")
    val v1 = BerTLVCons(BerTag("A5"), List(v0, v0))
    val v2 = BerTLVCons(BerTag("70"), List(v1, v1, v1))

    val input = "701EA5088002000080020000A5088002000080020000A508800200008002000080020000701EA5088002000080020000A5088002000080020000A508800200008002000080020000"
    val parser = new TLVParsers()
    val r = parser.parseTLVList(input).get match {
      case v : List[BerTLV] => v
      case _ => fail("Should result in a list of TLV: ")
    }
    r.length should be(4)
    r(0) should be (v2)
    r(1) should be (v0)
    r(2) should be (v2)
    r(3) should be (v0)
  }

  it should "be possible to pretty print TLV list" in {
    val input = "701EA5088002000080020000A5088002000080020000A508800200008002000080020000701EA5088002000080020000A5088002000080020000A508800200008002000080020000"
    val parser = new TLVParsers()
    val r = parser.parseTLVList(input).get
    val sss =  r.map(_.pretty).mkString.filter((c: Char) => c != '\n' && c != ' ' && c != '\t')
    sss should be("70A5800000800000A5800000800000A580000080000080000070A5800000800000A5800000800000A5800000800000800000")
  }



  //todo add negative testcases for parsing

}
