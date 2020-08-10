import java.nio.{ByteBuffer, ByteOrder}

import scala.reflect.ClassTag

/**
  * Created by anranw on 5/4/18.
  */
package object utils {
  def int(x: Float) = (if (x > -0.001f) x + 0.001f else x - 0.001f).asInstanceOf[Int]

  def int(x: Double) = (if (x > -0.001) x + 0.001 else x - 0.001).asInstanceOf[Int]

  def argmax(x: Array[Float], len: Int = 0) = {
    var id = 0
    for (i <- 1 until (len + x.length - 1) % x.length + 1)
      if (x(i) > x(id)) id = i
    id
  }

  def argmin(x: Array[Float], start: Int = 0, len: Int = 0) = {
    var id = start
    val _len = if (len > 0) Math.min(x.length - start, len) else x.length - start
    for (i <- 1 until _len)
      if (x(i + start) < x(id)) id = i + start
    id
  }

  def getAngle(i: Float, q: Float) = Math.atan2(q, i).toFloat

  case class IQ(amp: Float, angle: Float) {
    def *(iq: IQ) = IQ(amp * iq.amp, angle + iq.angle)

    def *(c: Float) = IQ(amp * c, angle)

    def +(iq: IQ) = {
      val i = amp * Math.cos(angle)
      val q = amp * Math.sin(angle)
      val i2 = iq.amp * Math.cos(iq.angle)
      val q2 = iq.amp * Math.sin(iq.angle)
      val i3 = i + i2
      val q3 = q + q2
      IQ(Math.sqrt(i3 * i3 + q3 * q3).asInstanceOf[Float], Math.atan2(q3, i3).asInstanceOf[Float])
    }

    def -(iq: IQ) = this + IQ(-iq.amp, iq.angle)

    def shift(delta: Float) = IQ(amp, delta + angle)

    def getI = Math.cos(angle).toFloat * amp

    def getQ = Math.sin(angle).toFloat * amp
  }

  object IQ {
    def fromIQ(I: Float, Q: Float) = IQ(Math.sqrt(I * I + Q * Q).asInstanceOf[Float], Math.atan2(Q, I)
      .asInstanceOf[Float])
  }


  case class EnrichedArrayFactory[T: ClassTag](default: T, plus: (T, T) => T, minus: (T, T) => T, mult: (T,
    Float) => T, mult2: (T, T) => T) {

    class EnrichedArray(val arr: Array[T]) {
      def +(arr2: Array[T]) = {
        val res = new Array[T](arr.length)
        for (i <- arr.indices) res(i) = EnrichedArrayFactory.this.plus(arr(i), arr2(i))
        res
      }

      def *(arr2: Array[T]) = {
        val res = new Array[T](arr.length)
        for (i <- arr.indices) res(i) = EnrichedArrayFactory.this.mult2(arr(i), arr2(i))
        res
      }

      def -(arr2: Array[T]) = {
        val res = new Array[T](arr.length)
        for (i <- arr.indices) res(i) = minus(arr(i), arr2(i))
        res
      }

      def *(c: Float) = {
        val res = new Array[T](arr.length)
        for (i <- arr.indices) res(i) = mult(arr(i), c)
        res
      }

      def +=(arr2: Array[T]) {
        for (i <- arr.indices) arr(i) = plus(arr(i), arr2(i))
      }

      def -=(arr2: Array[T]) {
        for (i <- arr.indices) arr(i) = minus(arr(i), arr2(i))
      }

      def reset(): Unit = {
        for (i <- arr.indices) arr(i) = default
      }

      def reverseLocal(from:Int, until:Int=arr.length): Unit ={
        var l=from
        var r=until-1
        while(l<r){
          val t=arr(l)
          arr(l)=arr(r)
          arr(r)=t
          l+=1
          r-=1
        }
      }
      def shift(offset:Int)={
        reverseLocal(0, offset)
        reverseLocal(offset)
        reverseLocal(0)
      }
    }

    def build(arr: Array[T]) = new EnrichedArray(arr)
  }

  implicit def enrichedArray(arr: Array[Float]) = EnrichedArrayFactory[Float](0f, _ + _, _ - _, _ * _, _ *
    _).build(arr)

  implicit def enrichedIQs(arr: Array[IQ]) = EnrichedArrayFactory[IQ](IQ(0, 0), _ + _, _ - _, _ * _, _ * _)
    .build(arr)


  def printsig(sig: Array[Float]): Unit = {
    print(sig(0))
    for (i <- 1 until sig.length)
      print("\t" + sig(i))
    println()
  }

  case class Position(x: Float, y: Float, z: Float) {
    def -(p: Position) = p match {
      case Position(_x, _y, _z) => Position(x - _x, y - _y, z - _z)
    }

    def +(p: Position) = p match {
      case Position(_x, _y, _z) => Position(x + _x, y + _y, z + _z)
    }

    def *(p: Float) = Position(x*p, y*p, z*p)

    def dist = Math.sqrt(x * x + y * y + z * z).toFloat
  }

  def floatSerialize(arr: Array[Float]) = {
    val bytearr = ByteBuffer.allocate(arr.length * 4)
    bytearr.asFloatBuffer().put(arr)
    bytearr.position(0)
    bytearr
  }

  def floatDeserialize(arr: Array[Byte]) = {
    val bytearr = ByteBuffer.wrap(arr)
    val floatbuf=bytearr.asFloatBuffer()
    val result=new Array[Float](floatbuf.limit)
    floatbuf.get(result)
    result
  }

  def shortDeserialize(arr:Array[Byte])={
    val bytearr=ByteBuffer.wrap(arr)
    bytearr.order(ByteOrder.LITTLE_ENDIAN)
    val shortbuf=bytearr.asShortBuffer()
    val result=new Array[Short](shortbuf.limit)
    shortbuf.get(result)
    result
  }

  def long2byte(l: Long) = {
    val buffer = ByteBuffer.allocate(8)
    buffer.putLong(l)
    buffer.array()
  }

  def byte2long(b: Array[Byte]) = {
    val buffer = ByteBuffer.wrap(b)
    buffer.getLong()
  }

  def log(s: Any): Unit = {
    println("LOG: " + s.toString)
  }

  def serializeIQs(arr: Array[IQ]) = {
    val amp = new Array[Float](arr.length)
    val ph = new Array[Float](arr.length)
    for (i <- arr.indices) {
      amp(i) = arr(i).amp
      ph(i) = arr(i).angle
    }
    (floatSerialize(amp), floatSerialize(ph))
  }
}
