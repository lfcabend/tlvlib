package org.tlv

import org.tlv.TLV.BerTLVCons

/**
  * Created by lau on 11-7-15.
  */
object HexUtils {

  def hex2Bytes(hex: String): Array[Byte] = {
    (for {i <- 0 to hex.length - 1 by 2 if i > 0 || !hex.startsWith("0x")}
      yield hex.substring(i, i + 2))
      .map(Integer.parseInt(_, 16).toByte).toArray
  }

  def toHex(b: Seq[Byte]): String = b.map("%02X" format _).mkString

  implicit def byteSeqToByteString(s: Seq[Byte]) = new ByteString(s)

  class ByteString(s: Seq[Byte]) {

    def toHex = org.tlv.HexUtils.toHex(s)

  }

  implicit def byteSeqToByteString(s: String) = new StringToByte(s)

  class StringToByte(s: String) {

    def fromHex = org.tlv.HexUtils.hex2Bytes(s)

  }

}
