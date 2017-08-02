package org.lau.tlv

import scodec.bits.ByteVector


trait Tag {

  val value: ByteVector

  lazy val length: Int = value.length.toInt

  override def toString = value.toHex

}

trait TLV[T0 <: Tag, T1 <: TLV[_, _]] extends Traversable[T1] {

  val tag: T0

  val value: ByteVector

  lazy val length: Int = value.length.toInt

  def serializeTLV: ByteVector

  def pretty: String

}