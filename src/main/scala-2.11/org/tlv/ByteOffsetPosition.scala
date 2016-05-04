package org.tlv

import scala.util.parsing.input.Position

/**
  * Created by Lau on 4/24/2016.
  */
case class ByteOffsetPosition(offset: Int) extends Position {
  final val line = 1

  def column = offset + 1

  def lineContents: String = ""
}
