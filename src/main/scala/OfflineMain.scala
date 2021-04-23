import FMCW.{FMCWFilter, FMCWTrack}
import SerialMain.{InputAdapter, args}
import blocks.{BlockDetector, QuadSeparator, QuadSynchronization, SignalGenerator}
import config.AcousticProperty
import utils.{ClockSync, FIRFilter, Header, HeaderExtraction, Logger, int}

import java.io.{FileInputStream, FileReader, FileWriter}
import java.util.Scanner
import java.util.concurrent.ArrayBlockingQueue

object OfflineMain extends App {

  // Serial input from AP, serial input from speaker, output to sound, output to log [or stdin]
  val in_ap = new FileInputStream(args(0))
  val in_spk = new Scanner(new FileInputStream(args(1)))
  val out_alert = new FileWriter(args(2))
  val ALERT_BLOCK=1
  val ALERT_FULLBUFFER=2
  val ALERT_LOWSNR=3

  def alert(i:Int){
    out_alert.write(i+"\n")
    out_alert.flush()
  }

  if(args.length>3) Logger.setDest(args(3))
  Logger.setTag("Log")

  val dataBuff = new ArrayBlockingQueue[(Option[Header], Array[Float])](100)

  val generator = new SignalGenerator(true)
  val template = generator.generateTemplate()
  val SNR_MIN = 0

  val blockDetector=new BlockDetector(0.95f, int(1/AcousticProperty.FMCW_CHIRP_DURATION), AcousticProperty.BLOCK_THRESHOLD,
    t=>if(t) alert(ALERT_BLOCK)
  )

  val sync = new QuadSynchronization(template, SNR_MIN, 50)
  val seperator = new QuadSeparator(generator.getPhaseFunc(), AcousticProperty.TRANSITION, blockDetector)
  val fmcwFilters = Array.range(0, AcousticProperty.CONCURRENT_TX).map(i => new FMCWFilter(AcousticProperty.FMCW_CHIRP_DURATION * i / 5))
  Array.range(0, AcousticProperty.CONCURRENT_TX).map(i => fmcwFilters(i).setId(i))

  val fmcwTracker = new FMCWTrack()
  var off = -1


  val clockSync = new ClockSync(AcousticProperty.SYNC_ALPHA, AcousticProperty.SYNC_BETA)
  val trackThread = new Thread(() => {
    var first = true

    while (true) {
      val (header, data) = dataBuff.take()
      header.foreach(clockSync.inputHeader)
      header.foreach(_.speakerClk.foreach(clockSync.inputSpeaker))

      if (data.length == 0) {
        // missed packet
        fmcwFilters.foreach(_.skip(clockSync.getDrift()))
        header.foreach(h => clockSync.inputResult(h.accData._1, None))
      } else {
        if (first) {
          seperator.init(data)
          first = false
        }
        else {
          seperator.input(data, 0)
        }
      }
    }
  })

  trackThread.start()

  val buffer = new Array[Byte](AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE * 2)
  val pkt = new Array[Float](AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE)
  var arr = new Array[Float](AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE)
  var speakerClkCnt = 0

  val headerExtraction = new HeaderExtraction[Header](buffer.length, identity, { (idx, data, header) =>
    val speakerCntTot = int(AcousticProperty.SPEAKER_CLOCK_INTERVAL / AcousticProperty.FMCW_CHIRP_DURATION)
    speakerClkCnt += 1
    if (speakerClkCnt > speakerCntTot && in_spk.hasNextLine() && header.nonEmpty) {
      val clock = in_spk.nextLine().trim().toInt // microseconds per 50k samples
      Logger.writeln("Speaker Clk")
      header.foreach {
        _.speakerClk = Some(clock)
      }
      speakerClkCnt -= speakerCntTot
    }
    if (data == null) {
      Logger.writeln("dropped! " + header.get.seq)
      // dropped packet, still synced
      dataBuff.put((header, new Array[Float](0)))
    } else {

      val arr = utils.shortDeserialize(data).map(_ / 32768f)
      if (off < 0) {
        sync.input(arr) match {
          case Some((snr, idx)) =>
            Logger.writeln("Synced. SNR: " + snr)
            if(snr<AcousticProperty.SNR_THRESHOLD)
              alert(ALERT_LOWSNR)

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
  })

  var left_preheat = AcousticProperty.PREHEAT
  while (true) {
    val len = in_ap.read(buffer, 0, buffer.length)
    if (left_preheat == 0) {
      if (len == buffer.length)
        headerExtraction.detectHeader(buffer)
    } else left_preheat -= 1
  }
}
