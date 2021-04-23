import java.io.{FileInputStream, FileWriter}
import java.util.Scanner
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}
import FMCW.{FMCWFilter, FMCWTrack}
import blocks.{QuadSeparator, QuadSynchronization, SignalGenerator}
import com.fazecast.jSerialComm._
import config.AcousticProperty
import utils.{ClockSync, FIRFilter, Header, HeaderExtraction, int}

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

    def getInputStream()=if(serial)port.getInputStream else file

    def readBytes(buffer:Array[Byte], length:Int)= {
      if (serial)
        port.readBytes(buffer, length)
      else
        file.read(buffer, 0, length)
    }
  }


  val dataBuff = new ArrayBlockingQueue[(Option[Header], Array[Float])](100)
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


  val clockSync=new ClockSync(AcousticProperty.SYNC_ALPHA, AcousticProperty.SYNC_BETA)
  val trackThread = new Thread(() => {
    var first = true

    //test output
    val fileout=new FileWriter("data/raw.txt")

    // test timing adjustment
    val adjout=new FileWriter("data/adj.txt")

    val resultout = Array.range(0, AcousticProperty.CONCURRENT_TX).map {id=>new FileWriter("data/fmcwresult"+id.toString() + ".txt")}
    val freqestout = Array.range(0, AcousticProperty.CONCURRENT_TX).map {id=>new FileWriter("data/freqresult"+id.toString() + ".txt")}
    while (!stop) {
      val (header, data) = dataBuff.take()
      header.foreach(clockSync.inputHeader)
      header.foreach(_.speakerClk.foreach(clockSync.inputSpeaker))

      //test
      adjout.write(clockSync.getAdjustment()+"\t"+fmcwFilters(0).getTotalDriftTime()+"\n")
      adjout.flush()

      if(data.length==0){
        // missed packet
        fmcwFilters.foreach(_.skip(clockSync.getDrift()))
        header.foreach(h=>clockSync.inputResult(h.accData._1, None))
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

          //test
          for (i<-0 until AcousticProperty.CONCURRENT_TX){
            freqestout(i).write(phres(i).freqEst+"\t")
            freqestout(i).flush()
          }

          //feedback drift
          val avgDist=tms.map(_.getDistance()).sum / tms.length
          header.foreach(h=>clockSync.inputResult(h.accData._1, Some(avgDist)))

          //update peak
          for(i<-0 until AcousticProperty.CONCURRENT_TX)
            fmcwFilters(i).updatePeak(tms(i).rawFreq)

          //test output
          for (i<-0 until AcousticProperty.CONCURRENT_TX) {
            resultout(i).write(tms(i).getDistance()+"\t")
            resultout(i).flush()
          }


          //output
          for (i <- 0 until AcousticProperty.CONCURRENT_TX)
            print(tms(i).getDistance() + "\t")
          println(header.get.seq)
        }
      }
    }
  })

  val input=if(args(0).equals("serial")) InputAdapter(true, args(1)) else InputAdapter(false, args(1))
  //val input=InputAdapter(true, args(0))
  var speakerClkSource:Scanner=null
  if(args.length>2) {
    if (args(2).forall(_.isDigit)) {
      clockSync.inputSpeaker(args(2).toInt)
    } else {
      val speakerClk = if (args(0).equals("serial")) InputAdapter(true, args(2)) else InputAdapter(false, args(2))
      speakerClkSource = new Scanner(speakerClk.getInputStream())
    }
  }

  trackThread.start()

  val buffer = new Array[Byte](AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE * 2)
  val pkt=new Array[Float](AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE)
  val filter=new FIRFilter(AcousticProperty.SR).getFIRFilter(20, 16000, 24000)
  var lastRaw:Array[Float]=null
  var arr=new Array[Float](AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE)
  var speakerClkCnt=0

  val headerExtraction=new HeaderExtraction[Header](buffer.length, identity, { (idx, data, header) =>
    val speakerCntTot=int(AcousticProperty.SPEAKER_CLOCK_INTERVAL/AcousticProperty.FMCW_CHIRP_DURATION)
    speakerClkCnt+=1
    if (speakerClkSource!=null && speakerClkCnt>speakerCntTot && speakerClkSource.hasNextLine() && header.nonEmpty){
      val clock = speakerClkSource.nextLine().trim().toInt // microseconds per 50k samples
      header.foreach{_.speakerClk=Some(clock)}
      speakerClkCnt-=speakerCntTot
    }
    if (data == null) {
      println("dropped! "+header.get.seq)
      // dropped packet, still synced
      dataBuff.put((header, new Array[Float](0)))
    } else {

      val raw_arr = utils.shortDeserialize(data).map(_ / 32768f)
      if(lastRaw!=null) {
        filter(raw_arr, lastRaw, arr)
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
          dataBuff.put((header, pkt.clone()))
          Array.copy(arr, AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE - off, pkt, 0, off)
        }
      }
      lastRaw=raw_arr
    }
  })

  var left_preheat=AcousticProperty.PREHEAT
  while (true) {
    val len = input.readBytes(buffer, buffer.length)
    if (left_preheat == 0) {
      if (len == buffer.length)
        headerExtraction.detectHeader(buffer)
    } else left_preheat-=1
  }
}
