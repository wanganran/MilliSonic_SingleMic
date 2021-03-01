import java.io.{FileInputStream, FileWriter}
import java.util.Scanner
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


  val dataBuff = new ArrayBlockingQueue[Array[Float]](100)
  var stop = false

  val generator = new SignalGenerator(true)
  val template = generator.generateTemplate()
  val SNR_MIN = 0

  val sync = new QuadSynchronization(template, SNR_MIN, 50)
  val seperator = new QuadSeparator(generator.getPhaseFunc(), AcousticProperty.TRANSITION)
  val fmcwFilters = Array.range(0, AcousticProperty.CONCURRENT_TX).map(i => new FMCWFilter(AcousticProperty.FMCW_CHIRP_DURATION * i / 5))
  Array.range(0, AcousticProperty.CONCURRENT_TX).map(i=>fmcwFilters(i).setId(i))

  val fmcwTracker = new FMCWTrack()
  var off = -1

  class ClockSync{
    val CLOCK=16*1000*1000.0
    val bufferSize=20
    val CLOCK_PER_SAMPLE=CLOCK/AcousticProperty.SR
    var timePerSamples=List[Double]()
    def ready()={
      timePerSamples.length>=bufferSize-1
    }
    var lastSeq = -1
    var lastTs = -1

    private def mod_diff(a:Int, b:Int, mod:Int)=if(a-b >= -mod/2 && a-b <= mod/2) a-b else if(a-b < -mod/2) a-b+mod else a-b-mod

    var microPerSecond=List[Float]()
    var totalDuration=0
    var totalSamples=0

    //test
    var idx=0

    def inputHeader(h:Header): Unit ={
      if(lastSeq>=0 && h.seq==(lastSeq+1)%256){
        lastSeq=h.seq

        if(idx>100) {
          totalDuration += h.timestamp - lastTs
          totalSamples += h.lastPacketLen
        }
        idx+=1

        timePerSamples:+=(h.timestamp-lastTs)/(h.lastPacketLen+Header.LENGTH).toDouble*2 // clock per sample
        //println("Header detected: "+h)
        println("timediff: "+h.seq+"\t"+(h.timestamp-lastTs)+"\t"+(h.timestamp))
        lastTs=h.timestamp
        if(timePerSamples.length==bufferSize)
          timePerSamples=timePerSamples.tail

      } else {
        // packet loss
        lastSeq=h.seq
        lastTs=h.timestamp
      }
    }
    def inputSpeaker(t:Int): Unit ={
      microPerSecond:+=t
      if(microPerSecond.length==bufferSize)
        microPerSecond=microPerSecond.tail
    }
    def getDrift()={
      if(!ready()) throw new IllegalArgumentException("Not enough time sync info accumulated")
      val avg=timePerSamples.sorted.slice(1, bufferSize-1).sum/(bufferSize-2)
      val micro=if(microPerSecond.length>0) microPerSecond.last/1e6d else 1
      val drift=avg/(CLOCK_PER_SAMPLE)*micro
      println("drift: "+drift)
      //drift-1
      val res=((1.000000381*micro)-1).toFloat
      //print(res)
      res
    }

    def getTotalDrift()={
      totalDuration*1f/totalSamples*2/(CLOCK_PER_SAMPLE)
    }
  }

  val clockSync=new ClockSync()
  val trackThread = new Thread(() => {
    var first = true

    //test output
    val fileout=new FileWriter("data/raw.txt")

    val resultout = Array.range(0, AcousticProperty.CONCURRENT_TX).map {id=>new FileWriter("data/fmcwresult"+id.toString() + ".txt")}

    while (!stop) {
      val data = dataBuff.take()
      if(data.length==0){
        // missed packet
        fmcwFilters.foreach(_.skip(clockSync.getDrift()))
      } else {
        if (first) {
          seperator.init(data)
          first = false
        }
        else {
          //test output
          for (d <- data)
            fileout.write(d.toString() + "\t")
          fileout.flush()


          val sepres = seperator.input(data, 0)
          val phres = for (i <- 0 until AcousticProperty.CONCURRENT_TX) yield {
            fmcwFilters(i).input(sepres(i), clockSync.getDrift())
          }
          val tms = fmcwTracker.getTm(phres.map(_.phases).toArray)

          //test output
          for (i<-0 until AcousticProperty.CONCURRENT_TX) {
            resultout(i).write(tms(i).getDistance()+"\t")
            resultout(i).flush()
          }


          //output
          for (i <- 0 until AcousticProperty.CONCURRENT_TX)
            print(tms(i).getDistance() + "\t")
          for (i <- 0 until AcousticProperty.CONCURRENT_TX)
            print(tms(i).getVelocity() + "\t")
          println()
        }
      }
    }
  })

  val input=if(args(0).equals("serial")) InputAdapter(true, args(1)) else InputAdapter(false, args(1))
  //val input=InputAdapter(true, args(0))
  if(args.length>2){
    if(args(2).forall(_.isDigit)){
      clockSync.inputSpeaker(args(2).toInt)
    } else {
      val serialSpeaker = SerialPort.getCommPort(args(2))
      serialSpeaker.openPort()
      serialSpeaker.setComPortTimeouts(SerialPort.TIMEOUT_READ_BLOCKING, 0, 0)
      val speakerThread = new Thread(() => {
        val stream = new Scanner(serialSpeaker.getInputStream())
        while (true) {
          if (stream.hasNextLine()) {
            val clock = stream.nextLine().trim().toInt // microseconds per 50k samples
            clockSync.inputSpeaker(clock)
          }
        }
      })
    }
  }

  trackThread.start()

  val buffer = new Array[Byte](AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE * 2)
  val pkt=new Array[Float](AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE)
  val headerExtraction=new HeaderExtraction(buffer.length,{h=>clockSync.inputHeader(h)}, {(idx, data)=>
    if(clockSync.ready()) {
      //test
      println("totaldrift: "+clockSync.getTotalDrift())
      if(data==null){
        println("dropped!")
        // dropped packet, still synced
        dataBuff.put(new Array[Float](0))
      } else {
        val arr = utils.shortDeserialize(data).map(_ / 32768f)
        if (off < 0) {
          sync.input(arr) match {
            case Some((snr, idx)) =>
              println("Synced. SNR: " + snr)
              off = idx
              Array.copy(arr, AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE - off, pkt, 0, off)
            case None =>
          }
        } else {
          Array.copy(arr, 0, pkt, off, AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE - off)
          dataBuff.put(pkt.clone())
          Array.copy(arr, AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE - off, pkt, 0, off)
        }
      }
    }
  })

  while (true) {
    val len = input.readBytes(buffer, buffer.length)
    assert(len == buffer.length)

    headerExtraction.detectHeader(buffer)
  }
}
