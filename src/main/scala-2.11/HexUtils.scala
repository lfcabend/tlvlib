/**
 * Created by lau on 11-7-15.
 */
object HexUtils {

  def hex2Bytes(hex: String): Array[Byte] = {
    (for {i <- 0 to hex.length - 1 by 2 if i > 0 || !hex.startsWith("0x")}
      yield hex.substring(i, i + 2))
      .map(Integer.parseInt(_, 16).toByte).toArray
  }

}