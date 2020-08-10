package utils

import scala.reflect.ClassTag

class BufferedArray[T:ClassTag](capacity:Int, init: ()=>T) {
  val buffer=Array.fill[T](capacity){init()}
  var head=0
  def get={
      val result=buffer(head)
      head=(head+1)%capacity
      result
  }
}
