package utils

case class Header(seq:Int, timestamp:Int, lastPacketLen:Int, byteIndex:Int){
  var accData:(Boolean, Short, Short, Short)=(true, 0, 0, 0)
}

object Header{
  val LENGTH=20 //Byte
  val DEFAULT_PKT_LEN=1024

  def parseAcc(header:Array[Int], offset:Int)={
    val move=(header(offset)!=0)
    val headerBuf=header.slice(offset+1, offset+7).map(_.toByte)
    val m=utils.shortDeserialize(headerBuf)
    (move, m(0), m(1), m(2))
  }
}
class HeaderExtraction[T] (outLen:Int, headerCallback:Header=>T, packetCallback:(Int, Array[Byte], Option[T])=>Unit) {
  // packetCallback: returns the number of bytes to skip
  assert(Header.DEFAULT_PKT_LEN<outLen)

  val preambleLen=8
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
  states(7)(0x00)=8
  states(7)(0x7f)=1

  val endState=preambleLen

  val buffer=new Array[Byte](outLen+preambleLen+1)
  var bufferPtr=0

  val headerBuffer=new Array[Int](Header.LENGTH-preambleLen)
  var headerPtr = -1
  var pktLen=0

  var byteId=0

  var lastHeaderId = -1
  var corrupted = false

  var currentHeaderInfo:Option[T]=None

  var state=0
  def detectHeader(sig:Array[Byte])={
    var i=0
    while(i<sig.length){
      if(headerPtr>=0){
        headerBuffer(headerPtr)=(sig(i) & 0xff)
        headerPtr+=1
        if(headerPtr==headerBuffer.length){
          // header found
          val seq=headerBuffer(0)
          val ts=(headerBuffer(1)|(headerBuffer(2)<<8)|(headerBuffer(3)<<16)|(headerBuffer(4)<<24))
          val lastPktLen=pktLen

          val acc=Header.parseAcc(headerBuffer, 5)
          val header=Header(seq, ts, lastPktLen, byteId)
          header.accData=acc
          currentHeaderInfo=Some(headerCallback(header))
          pktLen=0
          headerPtr = -1

          //check if missed packet
          if(lastHeaderId>=0 && (seq-lastHeaderId+256)%256!=1){
            corrupted=true
            val diff=(seq-lastHeaderId+256)%256
            //calc diff and skip byteId
            var skipped=(diff-1)*Header.DEFAULT_PKT_LEN
            while(skipped!=0){
              val next=buffer.length-bufferPtr
              if(next>skipped) {
                bufferPtr+=skipped
                byteId+=skipped
                skipped=0
              } else {
                byteId+=next
                packetCallback(byteId-buffer.length, null, currentHeaderInfo)
                bufferPtr=preambleLen+1
                skipped-=next
                if(skipped==0) corrupted=false
              }
            }
          }
          lastHeaderId=seq
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
          packetCallback(byteId-buffer.length, if(corrupted) null else buffer.slice(0, outLen), currentHeaderInfo)
          Array.copy(buffer, outLen, buffer, 0, preambleLen+1)
          bufferPtr=preambleLen+1
          corrupted=false
        }
      }
      i+=1
    }
  }

}
