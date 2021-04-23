package utils

import config.AcousticProperty


class MotionCalibration (xGap:Float, skipFirst:Int, caliDuration:Float, nonMotionDuration:Float, caliCallback:Float=>Unit) {
  var motionSeq=List[Option[Float]]()
  var nonMotionCounter=0
  val caliSeqLen=int(caliDuration/AcousticProperty.FMCW_CHIRP_DURATION)
  val nonMotionSeqLen=int(nonMotionDuration/AcousticProperty.FMCW_CHIRP_DURATION)

  private def fitLine() ={
    var b1 = 0f
    var b2 = 0f
    var meanx=0f
    var meany=0f
    var n=0
    for(i<-0 until motionSeq.length){
      motionSeq(i) match {
        case Some(x)=>
          meanx+=i*xGap
          meany+=x
          n+=1
      }
    }
    meanx/=n
    meany/=n
    for(i<-0 until motionSeq.length) {
      motionSeq(i) match {
        case Some(x) =>
          b1 += (i * xGap - meanx) * (x - meany)
          b2 += (i * xGap - meanx) * (i * xGap - meanx)
      }
    }
    val b = b1 / b2
    val a = meany - b * meanx
    (a,b)
  }

  type State=Int
  object State {
    val (start, stable, mobile, restable) = (0, 1, 2, 3)
  }
  var state:State=State.start

  def input(motion:Boolean, measuredDistance:Option[Float])= {
    state match {
      case State.start =>
        if (!motion) {
          motionSeq :+= measuredDistance
          if (motionSeq.flatten.size == caliSeqLen+skipFirst) {
            motionSeq=motionSeq.slice(skipFirst, motionSeq.length)
            //calculate drift now
            val (_,slope)=fitLine() // m/s
            val drift=slope/AcousticProperty.SOUND_SPEED //s/s
            caliCallback(-drift)
            motionSeq = List[Option[Float]]()
            state = State.stable
          }
        } else {
          if(motionSeq.flatten.nonEmpty) {
            motionSeq = List[Option[Float]]()
            state = State.mobile
          }
        }
      case State.stable =>
        if (motion)
          state = State.mobile
      case State.mobile =>
        if (!motion) {
          //after first calibration, then still
          //wait for nonMotionDuration second
          nonMotionCounter += 1
          if (nonMotionCounter == nonMotionSeqLen) {
            //start recalibration
            state = State.restable
          }
        } else {
          nonMotionCounter = 0
        }
      case State.restable =>
        if (motion) {
          motionSeq = List[Option[Float]]()
          state = State.mobile
        } else {
          motionSeq :+= measuredDistance
          if (motionSeq.flatten.size == caliSeqLen) {
            val (_,slope)=fitLine() // m/s
            val drift=slope/AcousticProperty.SOUND_SPEED //s/s
            caliCallback(-drift)
            motionSeq = List[Option[Float]]()
            state = State.stable
          }
        }
    }
  }
}
