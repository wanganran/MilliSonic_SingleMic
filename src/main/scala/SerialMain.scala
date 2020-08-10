import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}

import FMCW.{FMCWFilter, FMCWTrack}
import blocks.{QuadSeparator, QuadSynchronization, SignalGenerator}
import com.fazecast.jSerialComm._
import config.AcousticProperty

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


  val trackThread = new Thread(() => {
    var first = true
    while (!stop) {
      val data = dataBuff.take()
      if (first) seperator.init(data)
      else {
        val sepres = seperator.input(data, 0, 0)
        val phres = for (i <- 0 until 4) yield {
          //TODO: clock drift calculation
          fmcwFilters(i).input(sepres(i), 0f)
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
  while (true) {
    val len = port.readBytes(buffer, buffer.length)
    assert(len == buffer.length)
    //println("len "+len+"\t"+buffer(0))

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
}
