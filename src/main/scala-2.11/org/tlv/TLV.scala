package org.tlv

import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.immutable.Stream.cons
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

/**
  * Created by lau on 1-7-15.
  */

object TLV {

  trait Tag {

    val value: ByteVector

    lazy val length: Int = value.length.toInt

    override def toString() = value.toHex

  }

  trait TLV[T0 <: Tag, T1 <: TLV[_, _]] extends Traversable[T1] {

    val tag: T0

    val value: ByteVector

    lazy val length: Int = value.length.toInt

    def serializeTLV: ByteVector

    def pretty: String

  }

  case class BerTag(val value: ByteVector) extends Tag {
    require(Option(value).map(!_.isEmpty) == Option(true), "value is null or empty")

    private val hasNextByte: Seq[Boolean] =
      value.toArray.zipWithIndex.map { case (x, index) => shouldHaveNextByte(x, index) }

    require(hasNextByte.reduceRight(_ && _), "Invalid length indicators")

    def isConstructed: Boolean =
      value.lift(0).map(x => (x & 0x20) == 0x20).getOrElse(false)

    def isUniversalClass: Boolean =
      value.lift(0).map(x => (~x & 0xC0) == 0xC0).getOrElse(false)

    def isApplicationClass: Boolean =
      value.lift(0).map(x => (x & 0x40) == 0x40).getOrElse(false)

    def isContextSpecifClass: Boolean =
      value.lift(0).map(x => (x & 0x80) == 0x80).getOrElse(false)

    def isPrivateClass: Boolean =
      value.lift(0).map(x => (x & 0xC0) == 0xC0).getOrElse(false)

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

  abstract class PathX {

    def tag: BerTag

  }

  sealed case class PathEx(tag: BerTag) extends PathX

  sealed case class PathExIndex(tag: BerTag, index: Int = 0) extends PathX


  trait BerTLV extends TLV[BerTag, BerTLV] {

    def select(expression: List[PathX]): Option[List[BerTLV]] = selectInternal(expression)._1

    def selectLast(expression: List[PathX]): Option[List[BerTLV]] = selectLastInternal(expression, select(expression))

    def selectLastInternal(expression: List[PathX], r: Option[List[BerTLV]]): Option[List[BerTLV]] =
      r.map(_.flatMap(l => {
        (l, expression) match {
          case (v, (x :: Nil)) if v.tag == x.tag => List(v)
          case (v: BerTLVConsT, (x :: xs)) => v.constructedValue.flatMap(l => l.selectLastInternal(xs, Some(List(l))).getOrElse(Nil))
          case _ => Nil
        }
      }))

    def ->(expression: PathX*) = select(expression.toList)

    def selectInternal(expression: List[PathX]): (Option[List[BerTLV]], List[PathX])

    def prettyWithDepth(depth: Int): String

    def updated(tlv: BerTLV): BerTLV

    def updated(expression: List[PathX], tlv: BerTLV): BerTLV = updatedInternal(expression, tlv)._1

    def updatedInternal(expression: List[PathX], tlv: BerTLV): (BerTLV, List[PathX])

    def foldTLV[B](f: (BerTag, ByteVector) => B, g: (BerTag, Seq[B]) => B): B

    override def serializeTLV: ByteVector = {
      val f: (BerTag, ByteVector) => ByteVector =
        (t, v) => t.value ++ BerTLV.encodeLength(v) ++ v
      val g: (BerTag, Seq[ByteVector]) => ByteVector = (t, v) => {
        val cat: ByteVector = v.foldRight(ByteVector.empty)(_ ++ _)
        t.value ++ BerTLV.encodeLength(cat) ++ cat
      }
      foldTLV(f, g)
    }

  }

  object BerTLV {

    //    def encodeLengthOld(v: ByteVector): ByteVector = {
    //      val length = v.length
    //      if (length <= 0x7F)
    //        ByteVector(length.toByte)
    //      else if (length <= 0xFF)
    //        ByteVector(0x81, length)
    //      else if (length <= 0xFFFF)
    //        ByteVector(0x82.toByte, ((length >> 8) & 0xFF).toByte, (length & 0xFF).toByte)
    //      else if (length <= 0xFFFFFF)
    //        ByteVector(0x83.toByte, ((length >> 16) & 0xFF).toByte, ((length >> 8) & 0xFF).toByte, (length & 0xFF).toByte)
    //      else //if (length <= 0x7FFFFFFF) {
    //        ByteVector(0x84.toByte, ((length >> 24) & 0xFF).toByte,
    //          ((length >> 16) & 0xFF).toByte, ((length >> 8) & 0xFF).toByte, (length & 0xFF).toByte)
    //    }

    def encodeLength(v: ByteVector): ByteVector = {
      val length = v.length.toInt
      encodeLength(length)
    }

    def encodeLength(length: Int): ByteVector = {
      lazy val theRest: Int => ByteVector = x =>
        if (x == 0) ByteVector(length.toByte)
        else ByteVector((length >> (x * 8)) & 0xFF) ++ theRest(x - 1)
      val lengthEncoded: Int => ByteVector = x =>
        if (x > 0) ByteVector((0x80 | x).toByte) ++ theRest(x - 1)
        else theRest(x)

      //TODO change with log that might be expensive
      if (length <= 0x7F)
        lengthEncoded(0)
      else if (length <= 0xFF)
        lengthEncoded(1)
      else if (length <= 0xFFFF)
        lengthEncoded(2)
      else if (length <= 0xFFFFFF)
        lengthEncoded(3)
      else //if (length <= 0x7FFFFFFF) {
        lengthEncoded(4)
    }
  }

  implicit def byteVectorToPathEx(s: ByteVector) = new PathEx(new BerTag(s))

  implicit def ByteVectorAndIndexToPathEx(x: (ByteVector, Int)) = new PathExIndex(new BerTag(x._1), x._2)

  implicit def seqToTLVSeq(s: List[BerTLV]) = new TLVSeq(s)

  class TLVSeq(s: List[BerTLV]) {

    def >>:(tag: BerTag) = BerTLVCons(tag, s)

  }

  implicit def byteVectorToTLVValue(s: ByteVector) = new TLVValue(s)

  class TLVValue(s: ByteVector) {

    def >>:(tag: BerTag) = BerTLVLeaf(tag, s)

  }

  implicit def byteVectorToBerTag(s: ByteVector) = BerTag(s)


  implicit def tlvToTLVCons(tlv: BerTLV) = new TLVCons(tlv)

  class TLVCons(tlv: BerTLV) {

    def >>:(tag: BerTag) = BerTLVCons(tag, List(tlv))

  }


  implicit def getTag(tlv: List[BerTLV]) = new GetFromTLVList(tlv)

  class GetFromTLVList(tlv: List[BerTLV]) {

    def getTag(tag: BerTag): Option[BerTLV] = tlv.foldRight[Option[BerTLV]](None)({
      case (a, Some(x)) => Some(x)
      case (a, None) if (a.tag == tag) => Some(a)
      case _ => None
    })

  }

  implicit def toTlvMap(tlv: List[BerTLV]) = new TLVListToMap(tlv)

  class TLVListToMap(tlv: List[BerTLV]) {

    def toTlvMap(): Map[BerTag, BerTLV] = tlv.foldRight[Map[BerTag, BerTLV]](Map())((x, y) => y + (x.tag -> x))

  }

  case class BerTLVLeaf(val tag: BerTag, val value: ByteVector) extends BerTLVLeafT

  trait BerTLVLeafT extends BerTLV {

    //    require(tag != null, "tag is null")
    //    require(value != null, "value is null")


    override def updated(tlv: BerTLV): BerTLV =
      if (tlv.tag == tag) tlv
      else this

    override def toString() = s"BerTLVLeaf($tag, $value)"

    override def foreach[U](f: BerTLV => U): Unit = f(this)

    def foldTLV[B](f: (BerTag, ByteVector) => B, g: (BerTag, Seq[B]) => B): B = f(tag, value)


    override def pretty: String =
      tag.toString() + " " + value.toHex + "\n"

    def prettyWithDepth(depth: Int): String =
      "\t" * depth + pretty

    override def selectInternal(expression: List[PathX]): (Option[List[BerTLV]], List[PathX]) = expression match {
      case ((x: PathEx) :: Nil) if x.tag == tag =>
        (Some(List(this)), expression)
      case ((x: PathExIndex) :: Nil) if x.index == 0 && x.tag == tag =>
        (Some(List(this)), List(PathExIndex(x.tag, x.index - 1)))
      case ((x: PathExIndex) :: Nil) if x.index != 0 && x.tag == tag =>
        (None, List(PathExIndex(x.tag, x.index - 1)))
      case _ =>
        (None, expression)
    }

    override def updatedInternal(expression: List[PathX], tlv: BerTLV): (BerTLV, List[PathX]) = expression match {
      case ((x: PathEx) :: Nil) if x.tag == tag =>
        (tlv, expression)
      case ((x: PathExIndex) :: Nil) if x.index == 0 && x.tag == tag =>
        (tlv, List(PathExIndex(x.tag, x.index - 1)))
      case ((x: PathExIndex) :: Nil) if x.index != 0 && x.tag == tag =>
        (this, List(PathExIndex(x.tag, x.index - 1)))
      case _ =>
        (this, expression)
    }

  }

  case class BerTLVCons(val tag: BerTag, val constructedValue: List[BerTLV]) extends BerTLVConsT {

    override def copyByConstructedValue(newConstructedValue: List[BerTLV]): BerTLVConsT =
      BerTLVCons(tag, newConstructedValue)

  }

  trait BerTLVConsT extends BerTLV {
    self =>
    //    require(tag != null, "tag is null")
    //    require(tag.isConstructed, "need a constructed tag")
    //    require(constructedValue != null, "value is null or empty")

    def constructedValue: List[BerTLV]

    override def toString() = s"BerTLVCons($tag, $constructedValue)"

    def copyByConstructedValue(newConstructedValue: List[BerTLV]): BerTLVConsT

    def foreach[U](f: BerTLV => U): Unit = {
      f(this)
      constructedValue.foreach({ x =>
        x.foreach(f)
      })
    }

    def foldTLV[B](f: (BerTag, ByteVector) => B, g: (BerTag, Seq[B]) => B): B = g(tag, constructedValue.map(_.foldTLV(f, g)))

    def updated(tlv: BerTLV): BerTLV =
      if (tlv.tag == tag) tlv
      else BerTLVCons(tag, constructedValue.map(_.updated(tlv)))

    override val value: ByteVector = constructedValue.foldRight(ByteVector.empty)((x, y) => x.serializeTLV ++ y)

    override def pretty: String = prettyWithDepth(0)

    override def prettyWithDepth(depth: Int): String =
      "\t" * depth + tag.toString() + "\n" + constructedValue.map(_.prettyWithDepth(depth + 1)).mkString

    override def selectInternal(expression: List[PathX]): (Option[List[BerTLV]], List[PathX]) = {
      //combine the two optional lists
      def combine(v1: Option[List[BerTLV]], v2: Option[List[BerTLV]]) = (v1, v2) match {
        case (Some(v1), Some(v2)) => Some(v1 ++ v2)
        case (None, Some(v2)) => Some(v2)
        case (Some(v1), None) => Some(v1)
        case _ => None
      }

      def foldFunc(tlv: BerTLV, currentResult: (Option[List[BerTLV]], List[PathX])) = {
        val (cTLV, _) = currentResult
        val (nTLV, nEx) = tlv.selectInternal(currentResult._2)
        (combine(cTLV, nTLV), nEx)
      }
      //when match
      val cons: Option[List[BerTLV]] => Option[List[BerTLV]] =
        x1 => x1.map(x => List(this.copyByConstructedValue(x)))
      //base cases when no match
      matchAllPathCases(new PathHandler[(Option[List[BerTLV]], List[PathX])](expression) {

        val z0: (Option[List[BerTLV]], List[PathX]) = (None, expression.tail)

        val z1: (Option[List[BerTLV]], List[PathX]) = (None, expression)

        override def caseLeafPathNoIndex(x: PathEx) =
          (Some(List(self)), x :: Nil)

        override def caseLeafPathLastIndex(x: PathExIndex) =
          (Some(List(self)), List(PathExIndex(x.tag, x.index - 1)))

        override def caseLeafPathWithIndex(x: PathExIndex): (Option[List[BerTLV]], List[PathX]) =
          (None, List(PathExIndex(x.tag, x.index - 1))) //maybe we should continue recursively since it can still occur

        override def caseConsMatchingTag(x: PathEx, xs: List[PathX]): (Option[List[BerTLV]], List[PathX]) = {
          val r = constructedValue.foldLeft(z0)((p1, p2) => foldFunc(p2, p1))
          (cons(r._1), x :: r._2)
        }

        override def caseConsMatchingTagLastIndex(x: PathExIndex, xs: List[PathX]) = {
          val r = constructedValue.foldLeft(z0)((p1, p2) => foldFunc(p2, p1))
          (cons(r._1), PathExIndex(x.tag, x.index - 1) :: r._2)
        }

        override def caseConsMatchingTagWithIndex(x: PathExIndex, xs: List[PathX]) = {
          (None, PathExIndex(x.tag, x.index - 1) :: xs) //maybe we should continue recursively since it can still occur
        }

        override def caseNoMatchingTag(x: PathX, xs: List[PathX]) =
          constructedValue.foldLeft(z1)((p1, p2) => foldFunc(p2, p1))

        override def defaultCase(): (Option[List[BerTLV]], List[PathX]) = (None, expression)
      })

    }

    private abstract class PathHandler[R0](val expression: List[PathX]) {


      def caseNoMatchingTag(x: PathX, xs: List[PathX]): R0

      def caseConsMatchingTagWithIndex(x: PathExIndex, xs: List[PathX]): R0

      def caseConsMatchingTagLastIndex(x: PathExIndex, xs: List[PathX]): R0

      def caseConsMatchingTag(x: PathEx, xs: List[PathX]): R0

      def caseLeafPathWithIndex(x: PathExIndex): R0

      def caseLeafPathLastIndex(x: PathExIndex): R0

      def caseLeafPathNoIndex(x: PathEx): R0

      def defaultCase(): R0

    }

    private def matchAllPathCases[R](handler: PathHandler[R]): R = handler.expression match {
      case ((x: PathEx) :: Nil) if x.tag == tag =>
        handler.caseLeafPathNoIndex(x)
      case ((x: PathExIndex) :: Nil) if x.index == 0 && x.tag == tag =>
        handler.caseLeafPathLastIndex(x)
      case ((x: PathExIndex) :: Nil) if x.index != 0 && x.tag == tag =>
        handler.caseLeafPathWithIndex(x)
      case ((x: PathEx) :: xs) if x.tag == tag =>
        handler.caseConsMatchingTag(x, xs)
      case ((x: PathExIndex) :: xs) if x.index == 0 && x.tag == tag =>
        handler.caseConsMatchingTagLastIndex(x, xs)
      case ((x: PathExIndex) :: xs) if x.index != 0 && x.tag == tag =>
        handler.caseConsMatchingTagWithIndex(x, xs)
      case ex@(x :: xs) if x.tag != tag =>
        handler.caseNoMatchingTag(x, xs)
      case _ =>
        handler.defaultCase()
    }

    override def updatedInternal(expression: List[PathX], tlv: BerTLV): (BerTLV, List[PathX]) = {
      def foldFunc(tlv: BerTLV, currentResult: (List[BerTLV], List[PathX])): (List[BerTLV], List[PathX]) = {
        val (cTLV, _) = currentResult
        val (nTLV, nEx) = tlv.updatedInternal(currentResult._2, tlv)
        (cTLV ++ nTLV, nEx)
      }

      matchAllPathCases(new PathHandler[(BerTLV, List[PathX])](expression) {

        val z0: (List[BerTLV], List[PathX]) = (Nil, expression.tail)
        val z1: (List[BerTLV], List[PathX]) = (Nil, expression)

        override def caseLeafPathNoIndex(x: PathEx) =
          (tlv, x :: Nil)

        override def caseLeafPathLastIndex(x: PathExIndex) =
          (tlv, List(PathExIndex(x.tag, x.index - 1)))

        override def caseLeafPathWithIndex(x: PathExIndex) =
          (self, List(PathExIndex(x.tag, x.index - 1))) //maybe we should continue recursively since it can still occur

        override def caseConsMatchingTag(x: PathEx, xs: List[PathX]) = {
          val (r1: List[BerTLV], r2: List[PathX]) = constructedValue.foldLeft(z0)((p1, p2) => foldFunc(p2, p1))
          (BerTLVCons(tag, r1), x :: r2)
        }

        override def caseConsMatchingTagLastIndex(x: PathExIndex, xs: List[PathX]) = {
          val r = constructedValue.foldLeft(z0)((p1, p2) => foldFunc(p2, p1))
          (BerTLVCons(tag, r._1), PathExIndex(x.tag, x.index - 1) :: r._2)
        }

        override def caseConsMatchingTagWithIndex(x: PathExIndex, xs: List[PathX]) = {
          (self, PathExIndex(x.tag, x.index - 1) :: xs) //maybe we should continue recursively since it can still occur
        }

        override def caseNoMatchingTag(x: PathX, xs: List[PathX]) = {
          val r = constructedValue.foldLeft(z1)((p1, p2) => foldFunc(p2, p1))
          (BerTLVCons(tag, r._1), r._2)
        }

        override def defaultCase() = (self, expression)

      })
    }

  }

  object Parser {

    import fastparse.byte.all._

    import fastparse.byte.all.Parser

    def parseSingleByteTag = P(AnyByte.!.map((x: Bytes) => BerTag(x)))

    def parseFirstByteOfTagWithMore = P(AnyByte.!.filter(x => BerTag.hasNextByte(x.toByte(), 0))).
      opaque("Byte did not indicate more bytes")

    def parseTwoByteTag = P(
      for (
        x1 <- parseFirstByteOfTagWithMore;
        x2 <- AnyByte.!
      ) yield {
        BerTag(x1 ++ x2)
      })

    def parseTag: Parser[BerTag] = P(parseMoreByteTag | parseTwoByteTag | parseSingleByteTag)

    def parseMoreByteTag = P(for (
      x1 <- parseFirstByteOfTagWithMore;
      x2 <- parseSubsequentByteOfTagWithMore.rep.!;
      x3 <- AnyByte.!
    ) yield {
      BerTag(x1 ++ x2 ++ x3)
    })

    def parseSubsequentByteOfTagWithMore = P(AnyByte.!.filter(x => BerTag.hasNextByte(x.toByte(), 1)))

    def parseSingleLength = P(AnyByte.!.filter(x => (x.toByte() & 0xFF) <= 0x7F).map(_.toInt()))

    def parseMultipleLength = P(for (
      x <- firstByteOfMultipleLength;
      y <- parserNBytesOfLength(x)
    ) yield {
      BigInt(1, y.toArray).intValue()
    })

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

    def parseTLVList = P(parseTLV.rep)

    def parseTLVLeaf = P(for (
      x1 <- parseNonConstructedATag;
      x2 <- parseLength;
      x3 <- AnyByte.!.rep(exactly = x2).!
    ) yield {
      BerTLVLeaf(x1, x3)
    })

    def parseTLVCons = P(for (
      x1 <- parseAConstructedTag;
      x2 <- parseLength;
      x3 <- repParsingForXByte(x2)
    ) yield {
      BerTLVCons(x1, x3)
    })

    def repParsingForXByte(totalSize: Int) = new RepParserForXBytes(totalSize)

    case class RepParserForXBytes(totalSize: Int) extends Parser[List[BerTLV]] {

      import fastparse.core._

      def parseRec(cfg: ParseCtx[Byte, ByteVector], index0: Int): Mutable[List[BerTLV], Byte, ByteVector] = {
        val elems = new ListBuffer[BerTLV]
        def continue(cfg: ParseCtx[Byte, ByteVector], index: Int, size: Int) = {
          @tailrec def applyP(cfg: ParseCtx[Byte, ByteVector], index: Int, size: Int): Mutable[List[BerTLV], Byte, ByteVector] = {
            parseTLV.parseRec(cfg, index) match {
              case Mutable.Success(r, i, t, c) if i - index0 < size => {
                elems += r;
                applyP(cfg, i, size)
              }
              case Mutable.Success(r, i, t, c) => {
                elems += r
                success(cfg.success, elems.toList, i, Set.empty, false)
              }
              case s: Mutable.Failure[Byte, ByteVector] => s
            }
          }
          applyP(cfg, index, size)
        }
        if (totalSize <= 0) {
          success(cfg.success, elems.toList, index0, Set.empty, false)
        } else {
          continue(cfg, index0, totalSize)
        }
      }

    }

  }


}
