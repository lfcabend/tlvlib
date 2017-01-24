package org.lau.tlv.ber

import scodec.bits.ByteVector
import org.lau.tlv._

case class BerTag(value: ByteVector) extends Tag {
  require(Option(value).map(!_.isEmpty) == Option(true), "value is null or empty")

  private val hasNextByte: Seq[Boolean] =
    value.toArray.zipWithIndex.map { case (x, index) => shouldHaveNextByte(x, index) }

  require(hasNextByte.reduceRight(_ && _), "Invalid length indicators")

  def isConstructed: Boolean =
    value.lift(0).exists(x => (x & 0x20) == 0x20)

  def isUniversalClass: Boolean =
    value.lift(0).exists(x => (~x & 0xC0) == 0xC0)

  def isApplicationClass: Boolean =
    value.lift(0).exists(x => (x & 0x40) == 0x40)

  def isContextSpecifClass: Boolean =
    value.lift(0).exists(x => (x & 0x80) == 0x80)

  def isPrivateClass: Boolean =
    value.lift(0).exists(x => (x & 0xC0) == 0xC0)

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


case class BerTLVLeaf(tag: BerTag, value: ByteVector) extends BerTLVLeafT

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

case class BerTLVCons(tag: BerTag, constructedValue: List[BerTLV]) extends BerTLVConsT {

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
    def combine(param1: Option[List[BerTLV]], param2: Option[List[BerTLV]]) = (param1, param2) match {
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
      x1 => x1.map(x => List(self.copyByConstructedValue(x)))
    //base cases when no match
    matchAllPathCases(new PathHandler[(Option[List[BerTLV]], List[PathX])](expression) {

      type SRT = (Option[List[BerTLV]], List[PathX])

      val z0 = (None, expression.tail)

      val z1 = (None, expression)

      override def caseLeafPathNoIndex(x: PathEx) =
        (Some(List(self)), x :: Nil)

      override def caseLeafPathLastIndex(x: PathExIndex) =
        (Some(List(self)), List(PathExIndex(x.tag, x.index - 1)))

      override def caseLeafPathWithIndex(x: PathExIndex): (Option[List[BerTLV]], List[PathX]) =
        (None, List(PathExIndex(x.tag, x.index - 1))) //maybe we should continue recursively since it can still occur

      override def caseConsMatchingTag(x: PathEx, xs: List[PathX]): (Option[List[BerTLV]], List[PathX]) = {
        val r = constructedValue.foldLeft[SRT](z0)((p1, p2) => foldFunc(p2, p1))
        (cons(r._1), x :: r._2)
      }

      override def caseConsMatchingTagLastIndex(x: PathExIndex, xs: List[PathX]) = {
        val r = constructedValue.foldLeft[SRT](z0)((p1, p2) => foldFunc(p2, p1))
        (cons(r._1), PathExIndex(x.tag, x.index - 1) :: r._2)
      }

      override def caseConsMatchingTagWithIndex(x: PathExIndex, xs: List[PathX]) = {
        (None, PathExIndex(x.tag, x.index - 1) :: xs) //maybe we should continue recursively since it can still occur
      }

      override def caseNoMatchingTag(x: PathX, xs: List[PathX]) =
        constructedValue.foldLeft[SRT](z1)((p1, p2) => foldFunc(p2, p1))

      override def defaultCase() = (None, expression)
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
    def foldFunc(tlv: BerTLV, currentResult: (List[BerTLV], List[PathX])) = {
      val (cTLV, _) = currentResult
      val (nTLV, nEx) = tlv.updatedInternal(currentResult._2, tlv)
      (cTLV ++ nTLV, nEx)
    }


    matchAllPathCases(new PathHandler[(BerTLV, List[PathX])](expression) {

      type URT = (List[BerTLV], List[PathX])

      val z0 = (Nil, expression.tail)
      val z1 = (Nil, expression)

      override def caseLeafPathNoIndex(x: PathEx) =
        (tlv, x :: Nil)

      override def caseLeafPathLastIndex(x: PathExIndex) =
        (tlv, List(PathExIndex(x.tag, x.index - 1)))

      override def caseLeafPathWithIndex(x: PathExIndex) =
        (self, List(PathExIndex(x.tag, x.index - 1))) //maybe we should continue recursively since it can still occur

      override def caseConsMatchingTag(x: PathEx, xs: List[PathX]) = {
        val (r1, r2) = constructedValue.foldLeft[URT](z0)((p1, p2) => foldFunc(p2, p1))
        (BerTLVCons(tag, r1), x :: r2)
      }

      override def caseConsMatchingTagLastIndex(x: PathExIndex, xs: List[PathX]) = {
        val (r1, r2) = constructedValue.foldLeft[URT](z0)((p1, p2) => foldFunc(p2, p1))
        (BerTLVCons(tag, r1), PathExIndex(x.tag, x.index - 1) :: r2)
      }

      override def caseConsMatchingTagWithIndex(x: PathExIndex, xs: List[PathX]) = {
        (self, PathExIndex(x.tag, x.index - 1) :: xs) //maybe we should continue recursively since it can still occur
      }

      override def caseNoMatchingTag(x: PathX, xs: List[PathX]) = {
        val (r1, r2) = constructedValue.foldLeft[URT](z1)((p1, p2) => foldFunc(p2, p1))
        (BerTLVCons(tag, r1), r2)
      }

      override def defaultCase() = (self, expression)

    })
  }

}
