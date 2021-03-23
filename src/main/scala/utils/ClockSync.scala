package utils

import config.AcousticProperty

import java.io.FileWriter

// clock sync based on fusion of motion detection, drift estimation and RF sync
// acoustic fusion once non-motion is detected (moving average using alpha)
// acoustic-RF fusion once a few seconds (moving average using beta)
class ClockSync (alpha:Float, beta:Float){
  val CLOCK=16*1000*1000.0
  val CLOCK_PER_SAMPLE=CLOCK/AcousticProperty.SR
  val CLOCK_PER_PACKET=int(CLOCK*Header.DEFAULT_PKT_LEN/2/AcousticProperty.SR)

  var currentDrift:Option[Double]=None
  var currentTimingDiff:Double=0f

  var motionCalibration=new MotionCalibration(AcousticProperty.FMCW_CHIRP_DURATION,
    AcousticProperty.CALIBRATION_DURATION,
    AcousticProperty.RESTABLE_DURATION, drift=>
    if(currentDrift.isEmpty) currentDrift=Some(drift) else currentDrift=Some(currentDrift.get*alpha+drift*(1-alpha))
  )

  var lastSeq = -1
  var lastTs = -1
  var accTimestamp:Option[Int]=None
  var accTimestampNum=0

  private def mod_diff(a:Int, b:Int, mod:Int)=
    if(a-b >= -mod/2 && a-b <= mod/2) a-b
    else if(a-b < -mod/2) a-b+mod
    else a-b-mod

  var accSpeakerClock=0
  var accSpeakerClockNum=0

  private var avgAdjustment=0d

  var headerCnt=0

  //test
  val motionLog=new FileWriter("motion.txt")

  def inputHeader(h:Header): Unit={
    //test
    h.accData match {
      case (motion, x,y,z)=>
        motionLog.write(if(motion) "1\t" else "0\t")
        motionLog.write(x+"\t"+y+"\t"+z+"\n")
        motionLog.flush()
    }

    // update headers
    val delta=Math.min(h.seq-lastSeq, h.seq+256-lastSeq)
    val tsShouldbe=delta*CLOCK_PER_PACKET
    lastSeq=h.seq
    if(accTimestamp.isEmpty){
      lastTs=h.timestamp
      accTimestamp=Some(0)
    } else {
      val duration=h.timestamp-lastTs
      val diff=duration-tsShouldbe
      lastTs=h.timestamp
      accTimestamp=Some(accTimestamp.get+diff)
      accTimestampNum+=delta
    }
  }
  def inputSpeaker(t:Int): Unit ={
    accSpeakerClock+=t
    accSpeakerClockNum+=1
  }

  def inputResult(motion:Boolean, avgDistance: Option[Float]): Unit ={
    motionCalibration.input(motion, avgDistance)
  }

  private def getTotalDuration=accTimestamp.getOrElse(0)/CLOCK

  // acoustic drift
  def getDrift()= {
    if (currentDrift.isEmpty) AcousticProperty.DEFAULT_DRIFT
    else currentDrift.get
  }

  //return the adjustment in second
  //TODO: not tested
  def getAdjustment()={
    val adj = if (accTimestamp.nonEmpty) {
      val accDiff = accTimestamp.get / CLOCK
      val speakerCorrect = {
        if (accSpeakerClockNum > 0) {
          val durationScale = getTotalDuration / accSpeakerClockNum * AcousticProperty.SPEAKER_CLOCK_INTERVAL
          val micro = accSpeakerClock.toFloat - 1e6f * AcousticProperty.SPEAKER_CLOCK_INTERVAL * accSpeakerClockNum
          val total = micro * durationScale
          total / 1e6 // in second
        } else 0
      }
      accDiff + speakerCorrect
    } else 0
    avgAdjustment = avgAdjustment * beta + (1 - beta) * adj
    avgAdjustment
  }
}
