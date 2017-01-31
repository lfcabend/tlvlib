package org.lau.tlv.dgi

import org.lau.tlv.{TLV, Tag}
import scodec.bits.ByteVector

/**
  * Created by lau on 1/9/17.
  */

case class DGITag(p1: Byte, p2: Byte) extends Tag {

  override val value: ByteVector = ByteVector(p1, p2)

  def isSFI: Boolean = p1 >= 0x01.toByte && p1 <= 0x1E.toByte

  def isEncrypted: Boolean = inRange(0x80.toByte, 0x8F.toByte)

  def inRange(lowerBound: Byte, upperBound: Byte): Boolean =
    p1 >= lowerBound && p1 <= upperBound

}

case class DGI(tag: DGITag, value: ByteVector) extends DGITrait

trait DGITrait extends TLV[DGITag, DGITrait] {
  self =>

  override def serializeTLV: ByteVector = tag.value ++ DGI.encodeLength(value) ++ value

  override def pretty: String = tag.toString() + " " + value.toHex + "\n"

  override def foreach[U](f: (DGITrait) => U): Unit = f(self)

}

object DGI {

  /**
    * On 1-byte in binary format if the length of data is from ‘00’ to ‘FE’ (0 to 254 bytes).
    * On 3-byte with the first byte set to ‘FF’ followed by 2 bytes in binary format
    * from ‘0000’ to ‘FFFE’ (0 to 65 534), e.g. ‘FF01AF’ indicates a length of 431 bytes.
    *
    * @param v value to encode the length for
    * @return
    */
  def encodeLength(v: ByteVector): ByteVector = {
    val length = v.length.toInt
    encodeLength(length)
  }

  def encodeLength(length: Int): ByteVector = {
    if (length <= 0xFE)
      ByteVector(length.toByte)
    else if (length <= 0xFFFE)
      ByteVector(0xFF.toByte, ((length >> 8) & 0xFF).toByte, (length & 0xFF).toByte)
    else
      ByteVector(-1.toByte)
  }
}