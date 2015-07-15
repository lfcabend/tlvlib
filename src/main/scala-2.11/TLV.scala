import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 * Created by lau on 1-7-15.
 */

object TLV {

  import scala.language.implicitConversions
  import scala.util.parsing.combinator._

  implicit def byteSequenceWrapper(s: String): Seq[Byte] = HexUtils.hex2Bytes(s)

  trait Tag {

    def value(): Seq[Byte]

    def length: Int = value.length

    override def toString() = value.map("%02X" format _).mkString

  }

  trait TLV[T0 <: Tag, T1 <: TLV[_, _]] extends Traversable[T1] {

    def tag(): T0

    def value: Seq[Byte]

    def length: Int = value.length

    def serializeTLV: Seq[Byte]

  }

  case class BerTag(value: Seq[Byte]) extends Tag {
    require(Option(value).map(!_.isEmpty) == Option(true), "value is null or empty")

    val hasNextByte: Seq[Boolean] = value.zipWithIndex.map { case (x, index) => shouldHaveNextByte(x, index) }
    require(hasNextByte.reduceRight(_ && _), "Invalid length indicators")

    def isConstructed: Boolean =
      value.lift(0).map(x => (x & 0x20) == 0x20).getOrElse(false)

    def isUniversalClass: Boolean =
      value.lift(0).map(x => (~x & 0xC0) == 0xC0).getOrElse(false)

    private def shouldHaveNextByte(x: Byte, index: Int) = {
      val size = value.length
      val hasNext = BerTag.hasNextByte(x, index)
      if (index < size - 1) hasNext else !hasNext
    }

  }

  object BerTag {

    def hasNextByte(x: Byte, index: Int) = index match {
      case 0 => (x & 0x1F) == 0x1F
      case _ => (x & 0x80) == 0x80
    }

  }

  abstract class BerTLV extends TLV[BerTag, BerTLV] {

    //    def select(tag: Tag); ???
    //
    //    def select(tag: Seq[Tag]); ???

    def foldTLV[B](f: (BerTag, Seq[Byte]) => B, g: (BerTag, Seq[B]) => B): B = this match {
      case BerTLVLeaf(t, v) => f(t, v)
      case BerTLVCons(t, l) => g(t, l.map(_.foldTLV(f, g)))
    }

    override def serializeTLV: Seq[Byte] = {
      val f: (BerTag, Seq[Byte]) => Seq[Byte] =
        (t, v) => t.value ++ BerTLV.encodeLength(v) ++ v
      val g: (BerTag, Seq[Seq[Byte]]) => Seq[Byte] = (t, v) => {
        val cat: Seq[Byte] = v.flatten
        t.value ++ BerTLV.encodeLength(cat) ++ cat
      }
      foldTLV(f, g)
    }

  }

  object BerTLV {

    def parseTLV(in: Seq[Byte]): BerTLV = ???

    def parseTLVs(in: Seq[Byte]): Seq[BerTLV] = ???


    def encodeLength(v: Seq[Byte]): Seq[Byte] = {
      val length = v.length
      if (length <= 0x7F) {
        Array[Byte](length.toByte)
      }
      else if (length <= 0xFF) {
        Array[Byte](0x81.toByte, length.toByte)
      }
      else if (length <= 0xFFFF) {
        Array[Byte](0x82.toByte, ((length >> 8) & 0xFF).toByte, (length & 0xFF).toByte)
      }
      else if (length <= 0xFFFFFF) {
        Array[Byte](0x83.toByte, ((length >> 16) & 0xFF).toByte, ((length >> 8) & 0xFF).toByte, (length & 0xFF).toByte)
      }
      else if (length <= 0x7FFFFFFF) {
        Array[Byte](0x84.toByte, ((length >> 24) & 0xFF).toByte,
          ((length >> 16) & 0xFF).toByte, ((length >> 8) & 0xFF).toByte, (length & 0xFF).toByte)
      } else {
        throw new IllegalArgumentException(s"Length not supported: $length")
      }
    }
  }

  implicit def seqToTLVSeq(s: Seq[BerTLV]) = new TLVSeq(s)

  class TLVSeq(s: Seq[BerTLV]) {

    def >>:(tag: BerTag) = BerTLVCons(tag, s)

  }

  implicit def stringToTLVValue(s: String) = new TLVValue(s)

  class TLVValue(s: String) {

    def >>:(tag: BerTag) = BerTLVLeaf(tag, HexUtils.hex2Bytes(s))

  }

  implicit def stringToBerTag(s: String) = BerTag(HexUtils.hex2Bytes(s))


  implicit def tlvToTLVCons(tlv: BerTLV) = new TLVCons(tlv)

  class TLVCons(tlv: BerTLV) {

    def >>:(tag: BerTag) = BerTLVCons(tag, List(tlv))

  }

  sealed case class BerTLVLeaf(tag: BerTag, value: Seq[Byte]) extends BerTLV {

    require(tag != null, "tag is null")
    require(Option(value).map(!_.isEmpty) == Option(true), "value is null or empty")

    override def toString() = s"BerTLVLeaf($tag, $value)"

    override def foreach[U](f: BerTLV => U): Unit = f(this)

    def foldTLV[B](f: BerTLVLeaf => B) = f(this)

  }

  sealed case class BerTLVCons(tag: BerTag, constructedValue: Seq[BerTLV]) extends BerTLV {
    require(tag != null, "tag is null")
    require(tag.isConstructed, "need a constructed tag")
    require(Option(constructedValue).map(!_.isEmpty) == Option(true), "value is null or empty")


    override def toString() = s"BerTLVCons($tag, $constructedValue)"

    def foreach[U](f: BerTLV => U): Unit = {
      f(this)
      constructedValue.foreach({ x =>
        x.foreach(f)
      })
    }

    override def value: Seq[Byte] = constructedValue.flatMap(x => x.serializeTLV)

  }

  class TLVParsers extends BinaryParsers {

    import scala.language.postfixOps
    import java.math.BigInteger

    def parseTag(in: String) = parse(parseATag, in)

    def parseLength(in: String) = parse(parseALength, in)

    def parseTLV(in: String) = parse(parseATLV, in)

    lazy val parseATLV: Parser[BerTLV] = parseAMultipleTLV | parseASingleTLV

    lazy val parseAMultipleTLV : Parser[BerTLVCons] = parseAConstructedTag ~
      parseALength.into(x => repParsingTLVForXByte(x)) ^^ {case t ~ c => BerTLVCons(t, c)}

    lazy val parseASingleTLV : Parser[BerTLVLeaf] = parseNonConstructedATag  ~ parseALength.
      into(x => repN(x, parseSingleByte)) ^^ {
        case t ~ v => BerTLVLeaf(t, v)
      }

    lazy val parseNonConstructedATag: Parser[BerTag] = parseATag.withFilter(!_.isConstructed)

    lazy val parseAConstructedTag: Parser[BerTag] = parseATag.withFilter(_.isConstructed)

    lazy val parseALength: Parser[Int] = (parseSingleLength ^^ (_.toInt)) |
      (parseMultipleLength ^^ (x => {new BigInteger(1, x.toArray).intValue()}))

    lazy val parseSingleLength = parseSingleByte.
      filter((x: Byte) => {(x & 0xFF) <= 0x7F}).withFailureMessage("Byte is more then 0x7F")

    lazy val parseMultipleLength = firstByteOfMultipleLength.into(parserNBytesOfLength)

    lazy val parserNBytesOfLength : (Int => Parser[List[Byte]])= (x => repN(x, parseSingleByte))

    lazy val firstByteOfMultipleLength = parseSingleByte.
      filter((x: Byte) => {(x & 0xFF) > 0x7F}).map((x: Byte) => {(x & 0xFF) & 0x7F}).
      withFailureMessage("Byte is not more then 0x7F")


    lazy val parseATag: Parser[BerTag] = parseMoreByteTag | parseTwoByteTag | parseSingleByteTag

    lazy val parseMoreByteTag = parseFirstByteOfTagWithMore ~ (parseSubsequentByteOfTagWithMore*) ~ parseSingleByte  ^^ {
      case f ~ s ~ l => BerTag(f :: s ++ List(l))
    }

    lazy val parseTwoByteTag = parseFirstByteOfTagWithMore ~ parseSingleByte ^^ {case f ~ l => BerTag(f :: List(l))}

    lazy val parseSingleByteTag = parseSingleByte ^^ {case l => BerTag(List(l))}

    lazy val parseFirstByteOfTagWithMore = parseSingleByte.
      filter(BerTag.hasNextByte(_, 0)).withFailureMessage("Byte does not indicate more bytes")

    lazy val parseSubsequentByteOfTagWithMore = parseSingleByte.
      filter(BerTag.hasNextByte(_, 1)).withFailureMessage("Byte does not indicate more bytes")


    lazy val parseSingleByte: Parser[Byte] = new Parser[Byte] {
      def apply(in: Input): ParseResult[Byte] = {
        if (!in.atEnd) Success(in.first, in.rest)
        else Failure("End of stream", in)
      }
    }

    private def repParsingTLVForXByte(totalSize: Int): Parser[List[BerTLV]] = Parser { in =>
      val elems = new ListBuffer[BerTLV]

      def continue(in: Input): ParseResult[List[BerTLV]] = {
        @tailrec def applyP(in0: Input, currentSize: Int): ParseResult[List[BerTLV]] = parseATLV(in0) match {
          case Success(x, rest) if (x.serializeTLV.length + currentSize < totalSize) =>
            elems += x ; applyP(rest, x.size + currentSize)
          case e @ Error(_, _)  => e  // still have to propagate error
          case _                => Success(elems.toList, in0)
        }

        applyP(in, 0)
      }

      parseATLV(in) match {
        case Success(x, rest) => elems += x ; continue(rest)
        case ns: NoSuccess    => ns
      }
    }

  }


}