package org.tlv

import scala.util.parsing.input.CharArrayReader._
import scala.util.parsing.input.{Position, Reader}

/**
  * Created by Lau on 4/24/2016.
  */
class ByteReader(val bytes: Array[Byte], override val offset: Int) extends Reader[Byte] {
  def this(reader: Reader[_]) = this(reader.source.toString.getBytes, 0)

  def this(bytes: Seq[Byte]) = this(bytes.toArray, 0)

  def this(str: String) = this(HexUtils.hex2Bytes(str), 0)

  override def source = bytes map (_.toChar)

  def first: Byte = if (offset < bytes.length) bytes(offset) else EofCh.toByte

  def rest: ByteReader = if (offset < bytes.length) new ByteReader(bytes, offset + 1) else this

  def pos: Position = ByteOffsetPosition(offset)

  def atEnd = offset >= bytes.length

  def byteAt(n: Int) = bytes(n)

  def length = bytes.length - offset

  override def drop(n: Int): ByteReader = new ByteReader(bytes, offset + n)

  def take(n: Int): Seq[Byte] = bytes drop offset take n

  override def toString = "ByteReader(%d / %d)".format(offset, bytes.length)
}
