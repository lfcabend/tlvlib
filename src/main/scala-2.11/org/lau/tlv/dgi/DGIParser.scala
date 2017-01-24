package org.lau.tlv.dgi

/**
  * Created by lau on 1/9/17.
  */
object DGIParser {

  import fastparse.byte.all._

  def parseDGITag = P(for (
    p1 <- AnyByte.!;
    p2 <- AnyByte.!
  ) yield new DGITag(p1.toByte(), p2.toByte()))


  def parseDGILength = P(
    parseSingleByteLength | parseThreeByteLength
  )

  def parseSingleByteLength = P(
    AnyByte.!.filter(x => x.toByte() >= 0x01.toByte && x.toByte() <= 0xFE).map(_.toInt())
  )

  def parseThreeByteLength = P(for (
    p1 <- AnyByte.!.filter(x => x.toByte() == 0xFF.toByte);
    p2 <- AnyByte.!;
    p3 <- AnyByte.!
  ) yield (p2 ++ p3).toInt(signed = false))


  def parseDGI = P(for (
    tag <- parseDGITag;
    l <- parseDGILength;
    v <- AnyByte.!.rep(exactly = l).!
  ) yield DGI(tag, v))
}
