package org.lau.tlv

import scodec.bits.ByteVector

import scala.language.experimental.macros
import scala.language.implicitConversions
import fastparse.byte.all.Parser


/**
  * Created by lau on 1/9/17.
  */
package object ber {

  implicit def byteVectorToPathEx(s: BerTag): PathEx = new PathEx(s)

  implicit def ByteVectorAndIndexToPathEx(x: (BerTag, Int)): PathExIndex = new PathExIndex(x._1, x._2)

  implicit class BerTagContext(val sc: StringContext) {

    def berTag(args: ByteVector*): BerTag = macro Impl.berTagImpl

  }

  implicit class BerTLVContext(val sc: StringContext) {

    def berTLV(args: ByteVector*): BerTLV = macro Impl.berTLVImpl

  }

  implicit class BerTLVListContext(val sc: StringContext) {

    def berTLVList(args: ByteVector*): List[BerTLV] = macro Impl.berTLVListImpl

  }

  @deprecated("Provides compile time compatibility between 2.10 and 2.11", "1.0.6")
  object blackbox {
    type Context = scala.reflect.macros.Context
  }

  class Impl(val c:  blackbox.Context) {

    import c.{universe => u}
    import u._

    private def getPartLiterals[T](testValue: Parser[T],
                                   errMsg: String => String): List[String] = {

      val Apply(_, List(Apply(_, parts))) = c.prefix.tree
      parts map {
        case Literal(Constant(part: String)) =>
          val hex = ByteVector.fromHex(part)
          if (hex.isEmpty)
            c.error(c.enclosingPosition, "hexadecimal string literal may only contain characters [0-9a-fA-f]")
          else
            testValue.parse(hex.get) match {
              case fastparse.core.Parsed.Success(x, _) =>
              case (f@(_: Any)) => c.error(c.enclosingPosition, errMsg(f.toString()))
            }
          part
      }
    }

    def getHexInput(partLiterals: List[String])(args: Seq[c.Expr[ByteVector]]) = {
      val headPart = c.Expr[String](Literal(Constant(partLiterals.head)))
      val initialStringBuilder = reify {
        new StringBuilder().append(headPart.splice)
      }
      (args zip partLiterals.tail).foldLeft(initialStringBuilder) {
        case (sb, (arg, part)) =>
          val partExpr = c.Expr[String](Literal(Constant(part)))
          reify {
            sb.splice.append(arg.splice.toHex).append(partExpr.splice)
          }
      }
    }

    def getInputFromArgs[T](parser: Parser[T], errMsg: String => String, args: Seq[c.Expr[ByteVector]]) = {
      val partLiterals = getPartLiterals[T](parser, errMsg)
      getHexInput(partLiterals)(args)
    }

    def berTLVImpl(args: c.Expr[ByteVector]*): c.Expr[BerTLV] = {
      val parser = BerTLVParser.parseTLV
      val errMsg = (x: String) => s"Is not a valid ber TLV: ${x}"
      val sbInput = getInputFromArgs(parser, errMsg, args)
      reify {
        BerTLVParser.parseTLV.parse(ByteVector.fromValidHex(sbInput.splice.toString)).get.value
      }
    }

    def berTagImpl(args: c.Expr[ByteVector]*): c.Expr[BerTag] = {

      val parser = BerTLVParser.parseTag
      val errMsg = (x: String) => s"Is not a valid tag: $x"
      val sbInput = getInputFromArgs(parser, errMsg, args)
      reify {
        BerTLVParser.parseTag.parse(ByteVector.fromValidHex(sbInput.splice.toString)).get.value
      }
    }

    def berTLVListImpl(args: c.Expr[ByteVector]*): c.Expr[List[BerTLV]] = {
      val parser = BerTLVParser.parseTLVList
      val errMsg = (x: String) => s"Is not a valid tlv list: $x"
      val sbInput = getInputFromArgs(parser, errMsg, args)
      reify {
        BerTLVParser.parseTLVList.parse(ByteVector.fromValidHex(sbInput.splice.toString)).get.value.toList
      }
    }

  }

}
