package org.lau.tlv.ber

import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object BerTLVParser {

  import fastparse.byte.all._

  import fastparse.byte.all.Parser

  def parseSingleByteTag = P(AnyByte.!.filter(x => !BerTag.hasNextByte(x.toByte(), 0)).map((x: Bytes) => BerTag(x)))

  def parseFirstByteOfTagWithMore = P(AnyByte.!.filter(x => BerTag.hasNextByte(x.toByte(), 0))).
    opaque("Byte did not indicate more bytes")

  def parseTwoByteTag = P(
    for (
      x1 <- parseFirstByteOfTagWithMore.~/;
      x2 <- AnyByte.!
    ) yield BerTag(x1 ++ x2))

  def parseTag: Parser[BerTag] = P(parseMoreByteTag | parseTwoByteTag | parseSingleByteTag)

  def parseTag2End: Parser[BerTag] = P(parseTag ~ End)

  def parseMoreByteTag = P(for (
    x1 <- parseFirstByteOfTagWithMore.~/;
    x2 <- parseSubsequentByteOfTagWithMore.rep.!.~/;
    x3 <- AnyByte.!
  ) yield BerTag(x1 ++ x2 ++ x3))

  def parseSubsequentByteOfTagWithMore = P(AnyByte.!.filter(x => BerTag.hasNextByte(x.toByte(), 1)))

  def parseSingleLength = P(AnyByte.!.filter(x => (x.toByte() & 0xFF) <= 0x7F).map(_.toInt()))

  def parseMultipleLength = P(for (
    x <- firstByteOfMultipleLength.~/;
    y <- parserNBytesOfLength(x)
  ) yield BigInt(1, y.toArray).intValue())

  def parserNBytesOfLength = (x: Int) => P(AnyByte.!.rep(exactly = x).!)

  def firstByteOfMultipleLength = P(AnyByte.!.filter(x => {
    (x.toByte() & 0xFF) > 0x7F
  }).map(x => {
    (x.toByte() & 0xFF) & 0x7F
  }))

  def parseLength = P(parseSingleLength | parseMultipleLength)

  def parseNonConstructedATag: Parser[BerTag] = P(parseTag.filter(!_.isConstructed)).opaque("is not a non constructed tag")

  def parseAConstructedTag: Parser[BerTag] = P(parseTag.filter(_.isConstructed)).opaque("Is not a constructed tag")

  def parseTLV: Parser[BerTLV] = P(parseTLVCons | parseTLVLeaf)

  def parseTLV2End: Parser[BerTLV] = P(parseTLV ~ End)

  def parseTLVList = P(parseTLV.rep)

  def parseTLVList2End = P(parseTLV.rep ~ End)

  def parseTLVLeaf = P(for (
    x1 <- parseNonConstructedATag.~/;
    x2 <- parseLength.~/;
    x3 <- AnyByte.!.rep(exactly = x2).!
  ) yield {
    BerTLVLeaf(x1, x3)
  })

  def parseTLVCons = P(for (
    x1 <- parseAConstructedTag.~/;
    x2 <- parseLength.~/;
    x3 <- repParsingForXByte(x2)
  ) yield BerTLVCons(x1, x3))

  def repParsingForXByte(totalSize: Int) = new RepParserForXBytes(totalSize)

  case class RepParserForXBytes(totalSize: Int) extends Parser[List[BerTLV]] {

    import fastparse.core._

    type R = Mutable[List[BerTLV], Byte, ByteVector]

    def parseRec(cfg: ParseCtx[Byte, ByteVector], index0: Int): R = {
      val elems = new ListBuffer[BerTLV]
      def continue(cfg: ParseCtx[Byte, ByteVector], index: Int, size: Int) = {
        @tailrec def applyP(cfg: ParseCtx[Byte, ByteVector], index: Int, size: Int): R = {
          parseTLV.parseRec(cfg, index) match {
            case Mutable.Success(r, i, t, c) if i - index0 < size =>
              elems += r
              applyP(cfg, i, size)
            case Mutable.Success(r, i, t, c) =>
              elems += r
              success(cfg.success, elems.toList, i, Set.empty, cut = false)
            case s: Mutable.Failure[Byte, ByteVector] => s
          }
        }
        applyP(cfg, index, size)
      }
      if (totalSize <= 0) {
        success(cfg.success, elems.toList, index0, Set.empty, cut = false)
      } else {
        continue(cfg, index0, totalSize)
      }
    }

  }

}