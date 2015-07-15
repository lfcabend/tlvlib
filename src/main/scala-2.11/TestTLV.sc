import TLV._

//val parser = new TLVParsers
//
//parser.parseTLV("Df10810100")

val tlv:BerTLV = "70" >>: ("80" >>: "0000" :: Nil)

//tlv.filter(x: BerTlv => x == BerTag("80"))
