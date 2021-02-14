package utils

case class Header(seq:Int, timestamp:Int, lastPacketLen:Int, byteIndex:Int)
object Header{
  val LENGTH=12 //Byte
}
class HeaderExtraction (outLen:Int, headerCallback:Header=>Unit, packetCallback:(Int, Array[Byte])=>Unit) {

  val preambleLen=7
  val states=Array.fill(preambleLen){new Array[Int](256)}
  states(0)(0x7f)=1
  states(1)(0x7f)=1
  states(1)(0xff)=2
  states(2)(0x7f)=1
  states(2)(0x80)=3
  states(3)(0x7f)=1
  states(3)(0x00)=4
  states(4)(0x7f)=5
  states(5)(0x7f)=1
  states(5)(0xff)=6
  states(6)(0x7f)=1
  states(6)(0x80)=7
  states(6)(0x7f)=1

  val endState=7

  val buffer=new Array[Byte](outLen+preambleLen+1)
  var bufferPtr=0

  val headerBuffer=new Array[Int](5)
  var headerPtr = -1
  var pktLen=0

  var byteId=0

  def detectHeader(sig:Array[Byte])={
    var i=0
    var state=0
    while(i<sig.length){
      if(headerPtr>=0){
        headerBuffer(headerPtr)=(sig(i) & 0xff)
        headerPtr+=1
        if(headerPtr==headerBuffer.length){
          val seq=headerBuffer(0)
          val ts=(headerBuffer(1)|(headerBuffer(2)<<8)|(headerBuffer(3)<<16)|(headerBuffer(4)<<24))
          val lastPktLen=pktLen
          headerCallback(Header(seq, ts, lastPktLen, byteId))
          pktLen=0
          headerPtr = -1
        }
      } else {
        pktLen+=1
        buffer(bufferPtr)=sig(i)
        bufferPtr+=1
        byteId+=1
        state = states(state)(sig(i) & 0xff)
        if (state == endState) {
          //preamble detected
          headerPtr = 0
          state = 0
          pktLen-=preambleLen
          bufferPtr-=preambleLen
          byteId-=preambleLen
        }
        if(bufferPtr==buffer.length){
          packetCallback(byteId, buffer.slice(0, outLen))
          Array.copy(buffer, outLen, buffer, 0, preambleLen+1)
          bufferPtr=preambleLen+1
        }
      }
      i+=1
    }
  }

}
