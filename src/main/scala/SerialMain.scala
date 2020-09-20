import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}

import FMCW.{FMCWFilter, FMCWTrack}
import blocks.{QuadSeparator, QuadSynchronization, SignalGenerator}
import com.fazecast.jSerialComm._
import config.AcousticProperty
import utils.{Header, HeaderExtraction}

object SerialMain extends App {
  val portname = args(0)
  val port = SerialPort.getCommPort(portname)
  port.openPort()
  port.setComPortTimeouts(SerialPort.TIMEOUT_READ_BLOCKING, 0, 0)

  val dataBuff = new ArrayBlockingQueue[Array[Float]](100)
  var stop = false

  val generator = new SignalGenerator(true)
  val template = generator.generateTemplate()
  val SNR_MIN = 0

  val sync = new QuadSynchronization(template, SNR_MIN)
  val seperator = new QuadSeparator(generator.getPhaseFunc(), AcousticProperty.TRANSITION)
  val fmcwFilters = Array.range(0, 4).map(i => new FMCWFilter(AcousticProperty.FMCW_CHIRP_DURATION * i / 5))
  val fmcwTracker = new FMCWTrack()
  var off = -1

  class ClockSync{
    val bufferSize=20
    var timePerSamples=List[Float]()
    def ready()={
      timePerSamples.length<bufferSize
    }
    var lastSeq = -1
    var lastTs = -1
    def inputHeader(h:Header): Unit ={
      if(lastSeq>=0 && h.seq==(lastSeq+1)%256){
        lastSeq=h.seq
        timePerSamples:+=(h.timestamp-lastTs)/(h.lastPacketLen+Header.LENGTH).toFloat*2 - 1f/AcousticProperty.SR
        if(timePerSamples.length==bufferSize)
          timePerSamples=timePerSamples.tail
      }
    }
    def getDrift()={
      if(!ready()) throw new IllegalArgumentException("Not enough time sync info accumulated")
      val avg=timePerSamples.sorted.slice(1, bufferSize-1).sum/(bufferSize-2)
      avg
    }
  }

  val clockSync=new ClockSync()
  val trackThread = new Thread(() => {
    var first = true
    while (!stop) {
      val data = dataBuff.take()
      if (first) seperator.init(data)
      else {
        val sepres = seperator.input(data, 0, 0)
        val phres = for (i <- 0 until 4) yield {
          fmcwFilters(i).input(sepres(i), clockSync.getDrift())
        }
        val tms = fmcwTracker.getTm(phres.map(_.phases).toArray)

        //output
        for (i <- 0 until 4)
          print(tms(i).getDistance() + "\t")
        for (i <- 0 until 4)
          print(tms(i).getVelocity() + "\t")
        println()
      }
    }
  })

  trackThread.start()

  val buffer = new Array[Byte](AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE * 2)

  val headerExtraction=new HeaderExtraction(buffer.length,{h=>clockSync.inputHeader(h)}, {data=>
    if(clockSync.ready()) {
      val arr = utils.shortDeserialize(buffer).map(_ / 32768f)
      if (off < 0) {
        sync.input(Array(arr)) match {
          case Some((_, idx)) =>
            off = AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE - idx
            //skip the next off samples
            assert(2 * off == port.readBytes(buffer, 2 * off))
        }
      } else {
        dataBuff.put(arr)
      }
    }
  })

  while (true) {
    val len = port.readBytes(buffer, buffer.length)
    assert(len == buffer.length)
    //println("len "+len+"\t"+buffer(0))

    headerExtraction.detectHeader(buffer)
  }
}
