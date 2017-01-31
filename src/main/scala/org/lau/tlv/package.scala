package org.lau

import scala.language.implicitConversions

/**
  * Created by lau on 1/9/17.
  */
package object tlv {

  implicit def getTag[T <: Tag, V <: TLV[T, V]](tlv: List[V]) = new GetFromTLVList[T, V](tlv)

  class GetFromTLVList[T <: Tag, V <: TLV[T, V]](tlv: List[V]) {

    def getTag(tag: T): Option[V] = tlv.foldRight[Option[V]](None)({
      case (a, Some(x)) => Some(x)
      case (a, None) if a.tag == tag => Some(a)
      case _ => None
    })

  }


  implicit def toTlvMap[T <: Tag, V <: TLV[T, _]](tlv: List[V]) = new TLVListToMap[T, V](tlv)

  class TLVListToMap[T <: Tag, V <: TLV[T, _]](tlv: List[V]) {

    def toTlvMap: Map[T, V] = tlv.foldRight[Map[T, V]](Map())((x, y) => y + (x.tag -> x))

  }

}
