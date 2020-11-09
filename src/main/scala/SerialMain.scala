import java.io.FileInputStream
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}

import FMCW.{FMCWFilter, FMCWTrack}
import blocks.{QuadSeparator, QuadSynchronization, SignalGenerator}
import com.fazecast.jSerialComm._
import config.AcousticProperty
import utils.{Header, HeaderExtraction}

object SerialMain extends App {
  case class InputAdapter(serial:Boolean, path:String){
    var port:SerialPort=null
    var file:FileInputStream=null
    if(serial) {
      port = SerialPort.getCommPort(path)
      port.openPort()
      port.setComPortTimeouts(SerialPort.TIMEOUT_READ_BLOCKING, 0, 0)
    } else {
      file=new FileInputStream(path)
    }

    def readBytes(buffer:Array[Byte], length:Int)= {
      if (serial)
        port.readBytes(buffer, length)
      else
        file.read(buffer, 0, length)
    }
  }

  val input=if(args(0).equals("serial")) InputAdapter(true, args(1)) else InputAdapter(false, args(1))
  //val input=InputAdapter(true, args(0))



  val dataBuff = new ArrayBlockingQueue[Array[Float]](100)
  var stop = false

  val generator = new SignalGenerator(true)
  val template = generator.generateTemplate()
  val SNR_MIN = 0

  val sync = new QuadSynchronization(template, SNR_MIN, 50)
  val seperator = new QuadSeparator(generator.getPhaseFunc(), AcousticProperty.TRANSITION)
  val fmcwFilters = Array.range(0, 4).map(i => new FMCWFilter(AcousticProperty.FMCW_CHIRP_DURATION * i / 5))
  val fmcwTracker = new FMCWTrack()
  var off = -1

  class ClockSync{
    val CLOCK=16*1024*1024f
    val bufferSize=20
    val CLOCK_PER_SAMPLE=CLOCK/AcousticProperty.SR
    var timePerSamples=List[Float]()
    def ready()={
      timePerSamples.length>=bufferSize-1
    }
    var lastSeq = -1
    var lastTs = -1
    def inputHeader(h:Header): Unit ={
      if(lastSeq>=0 && h.seq==(lastSeq+1)%256){
        lastSeq=h.seq
        timePerSamples:+=(h.timestamp-lastTs)/(h.lastPacketLen+Header.LENGTH).toFloat*2 // clock per sample
        println("Header detected: "+h)
        lastTs=h.timestamp
        if(timePerSamples.length==bufferSize)
          timePerSamples=timePerSamples.tail
      } else {
        lastSeq=h.seq
        lastTs=h.timestamp
      }
    }
    def getDrift()={
      if(!ready()) throw new IllegalArgumentException("Not enough time sync info accumulated")
      val avg=timePerSamples.sorted.slice(1, bufferSize-1).sum/(bufferSize-2)
      avg/CLOCK_PER_SAMPLE-1
    }
  }

  val clockSync=new ClockSync()
  val trackThread = new Thread(() => {
    var first = true
    while (!stop) {
      val data = dataBuff.take()
      if (first) {
        seperator.init(data)
        first = false
      }
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
  val pkt=new Array[Float](AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE)
  val headerExtraction=new HeaderExtraction(buffer.length,{h=>clockSync.inputHeader(h)}, {data=>
    if(clockSync.ready()) {
      val arr = utils.shortDeserialize(buffer).map(_ / 32768f)
      if (off < 0) {
        sync.input(arr) match {
          case Some((snr, idx)) =>
            println("Synced. SNR: "+snr)
            off=idx
            Array.copy(arr, AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE-off, pkt, 0, off)
          case None =>
        }
      } else {
        Array.copy(arr, 0, pkt, off, AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE - off)
        dataBuff.put(pkt.clone())
        Array.copy(arr, AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE - off, pkt, 0, off)
      }
    }
  })

  while (true) {
    val len = input.readBytes(buffer, buffer.length)
    assert(len == buffer.length)

    headerExtraction.detectHeader(buffer)
  }
}
