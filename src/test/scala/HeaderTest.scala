import utils.{Header, HeaderExtraction}

object HeaderTest extends App {
  implicit def int2Byte(i: Int) = i.toByte
  val preamble=Array[Byte](0x7f, 0xff, 0x80, 0x00, 0x7f, 0xff, 0x80, 0x00)
  val sig1=Array.fill(656){(Math.random()*256).asInstanceOf[Byte]}
  val sig2=Array.fill(256){(Math.random()*256).asInstanceOf[Byte]}
  val sig3=Array.fill(535){(Math.random()*256).asInstanceOf[Byte]}
  val sig=sig1++sig2++sig3
  val signal=preamble ++ Array[Byte](0x00, 0x00, 0x00, 0x00, 0x00) ++
    sig1 ++
    preamble ++ Array[Byte](0x01, 0x01, 0x00, 0x00, 0x00) ++
    sig2 ++
    preamble ++ Array[Byte](0x02, 0x00, 0x01, 0x00, 0x00) ++
    sig3 ++
    preamble ++ Array[Byte](0x03, 0x00, 0x00, 0x01, 0x00)

  var sigptr=0
  val headerExtraction=new HeaderExtraction(600, {h:Header=>
      print(h)
    }, {(idx, d:Array[Byte])=>
    print(d.length)
      for(i<-0 until d.length) {
        if(sig(sigptr)!=d(i)) print("error @ "+i.toString())
        sigptr+=1
      }
  })

  headerExtraction.detectHeader(signal)
}
