package org.lau.tlv

import scodec.bits.ByteVector
import scodec.bits._

import scala.language.experimental.macros
import scala.language.implicitConversions
import fastparse.byte.all.Parser


/**
  * Created by lau on 1/9/17.
  */
package object ber {

  implicit def getTag(tlv: List[BerTLV]) = new GetFromTLVList(tlv)

  class GetFromTLVList(tlv: List[BerTLV]) {

    def getTag(tag: BerTag): Option[BerTLV] = tlv.foldRight[Option[BerTLV]](None)({
      case (a, Some(x)) => Some(x)
      case (a, None) if a.tag == tag => Some(a)
      case _ => None
    })

  }


  implicit def toTlvMap[T <: Tag, V <: TLV[T, _]](tlv: List[V]) = new TLVListToMap[T, V](tlv)

  class TLVListToMap[T <: Tag, V <: TLV[T, _]](tlv: List[V]) {

    def toTlvMap: Map[T, V] = tlv.foldRight[Map[T, V]](Map())((x, y) => y + (x.tag -> x))

  }

  implicit def byteVectorToPathEx(s: BerTag): PathEx = new PathEx(s)

  implicit def ByteVectorAndIndexToPathEx(x: (BerTag, Int)): PathExIndex = new PathExIndex(x._1, x._2)

  implicit class BerTagContext(val sc: StringContext) {

    def berTag(): BerTag = macro Impl.berTagImpl

  }

  implicit class BerTLVContext(val sc: StringContext) {

    def berTLV(): BerTLV = macro Impl.berTLVImpl

  }

  implicit class BerTLVListContext(val sc: StringContext) {

    def berTLVList(): List[BerTLV] = macro Impl.berTLVListImpl

  }

  @deprecated("Provides compile time compatibility between 2.10 and 2.11", "1.0.6")
  object blackbox {
    type Context = scala.reflect.macros.Context
  }

  class Impl(val c: blackbox.Context) {

    import c.{universe => u}
    import u._

    private def getPartLiterals() = {
      val Apply(_, List(Apply(_, parts))) = c.prefix.tree
      parts map {
        case Literal(Constant(part: String)) =>
          val hex = ByteVector.fromHex(part)
          if (hex.isEmpty)
            c.error(c.enclosingPosition, "hexadecimal string literal may only contain characters [0-9a-fA-f]")
          part
      }
    }

    def getHexInput(partLiterals: List[String]) = {
      val headPart = q"${partLiterals}.head"
      val initialStringBuilder = q"new StringBuilder().append(${headPart})"
      (partLiterals.tail).foldLeft(initialStringBuilder) {
        case (sb, arg) =>
          q"${sb}.append(${arg})"
      }
    }


    def getInputFromArgs[T](parser: Parser[T], errMsg: String => String) = {
      val partLiterals = getPartLiterals()
      val sb = getHexInput(partLiterals)
      val a = q"${sb}.toString()"
      val hex = c.eval[String](c.Expr(a))
      parser.parse(ByteVector.fromValidHex(hex)) match {
        case fastparse.core.Parsed.Success(x, _) =>
        case (f@(_: Any)) => c.error(c.enclosingPosition, errMsg(f.toString()))
      }
      a
    }

    def berTLVImpl(): c.Tree = {
      val parser = BerTLVParser.parseTLV
      val errMsg = (x: String) => s"Is not a valid ber TLV: ${x}"
      val sbInput = getInputFromArgs(parser, errMsg)
      q"BerTLVParser.parseTLV2End.parse(ByteVector.fromValidHex(${sbInput}.toString)).get.value"
    }

    def berTagImpl(): c.Tree = {

      val parser = BerTLVParser.parseTag
      val errMsg = (x: String) => s"Is not a valid tag: $x"
      val sbInput = getInputFromArgs(parser, errMsg)
      q"BerTLVParser.parseTag2End.parse(ByteVector.fromValidHex(${sbInput}.toString)).get.value"
    }

    def berTLVListImpl(): c.Tree = {
      val parser = BerTLVParser.parseTLVList
      val errMsg = (x: String) => s"Is not a valid tlv list: $x"
      val sbInput = getInputFromArgs(parser, errMsg)
      q"BerTLVParser.parseTLVList2End.parse(ByteVector.fromValidHex(${sbInput}.toString)).get.value.toList"
    }

  }

}
