import TLV._

//val parser = new TLVParsers
//
//parser.parseTLV("Df10810100")

//val tlv:BerTLV = "70" >>: ("80" >>: "0000" :: Nil)

//tlv.filter(x: BerTlv => x == BerTag("80"))

val v0 = BerTLVLeaf(BerTag("80"), "0000")
val v1 = BerTLVCons(BerTag("A5"), List(v0, v0))
val v2 = BerTLVCons(BerTag("70"), List(v1, v1, v1))
v2.serializeTLV.map("%02X" format _).mkString