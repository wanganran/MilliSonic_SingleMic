package FMCW

import config.AcousticProperty


class FMCWTrack {
  private val micConfig=AcousticProperty.FMCW_CHANNELS_CONFIG
  private val micConfigWIdx=micConfig.zipWithIndex
  private val verticalPairs=micConfigWIdx.groupBy{case ((x,y), idx)=>y}.map{case (y, arr)=>(y, arr.sortBy{case ((x,_), _)=>x})}
  private val horizontalPairs=micConfigWIdx.groupBy{case ((x,y), idx)=>x}.map{case (x, arr)=>(x, arr.sortBy{case ((_,y), _)=>y})}

  private val QUARTER1=AcousticProperty.FMCW_WINDOW_DURATION_SAMPLE/4
  private val QUARTER2=AcousticProperty.FMCW_WINDOW_DURATION_SAMPLE/2
  private val QUARTER3=3*AcousticProperty.FMCW_WINDOW_DURATION_SAMPLE/4

  private val OFFSET_SMOOTH=0.8f

  case class PosResult(avgToa:Float, velocity:Float){
    def getDistance()=avgToa*AcousticProperty.SOUND_SPEED
    def getVelocity()=velocity*AcousticProperty.SOUND_SPEED
  }

  //-pi to pi
  private def fitin2pi(f:Float)= {
    val raw2pi =
      if (f < 0) 2 * Math.PI.toFloat + (-f) % (2 * Math.PI.toFloat)
      else f % (2 * Math.PI.toFloat)
    if(raw2pi>Math.PI)
      raw2pi-2*Math.PI.toFloat
    else raw2pi
  }


  private class State{
    var velocity=0f //s/s
    var phaseOffset=0f
    var lastEndTm=0f
    var inited=false
    var lastEndPhase=0f
    var lastStartPhase=0f
  }

  private val MAXHISTORY=20

  private val states=Array.fill(micConfig.length)(new State())


  def reset(): Unit ={
    states.foreach{x=>
      x.velocity=0f
      x.phaseOffset=0f
      x.lastEndTm=0f
      x.inited=false
      x.lastEndPhase=0f
      x.lastStartPhase=0f
    }
  }

  private def estimateFreqInitPhase(phases:Array[Float])= {
    val meanx = AcousticProperty.FMCW_WINDOW_DURATION_SAMPLE / 2
    var meany = 0f
    for (i <- QUARTER1 to QUARTER3) meany += phases(i) / (AcousticProperty.FMCW_WINDOW_DURATION_SAMPLE / 2 + 1)
    var b1 = 0f
    var b2 = 0f
    for (i <- QUARTER1 to QUARTER3) {
      b1 += (i - meanx) * (phases(i) - meany)
      b2 += (i - meanx) * (i - meanx)
    }
    val b = b1 / b2
    val a = meany - b * meanx
    val freq = b * AcousticProperty.FMCW_WINDOW_DURATION_SAMPLE / Math.PI.toFloat / 2
    (freq, a)
  }

  def estimateFreqGivenTm(tm:Float)={
    val F=AcousticProperty.SINARR_FREQ_MAX-AcousticProperty.SINARR_FREQ_MIN
    val T=AcousticProperty.FMCW_CHIRP_DURATION
    val freq=tm*AcousticProperty.FMCW_WINDOW_DURATION*F/T
    freq
  }

  def estimatePhaseGivenTm(tm:Float, portion:Float)={
    val F=AcousticProperty.SINARR_FREQ_MAX-AcousticProperty.SINARR_FREQ_MIN
    val T=AcousticProperty.FMCW_CHIRP_DURATION
    val tT=portion*AcousticProperty.FMCW_WINDOW_RATIO
    val A=tT*2*Math.PI.toFloat*F+2*Math.PI.toFloat*AcousticProperty.SINARR_FREQ_MIN
    val C= Math.PI.toFloat*F/T
    val phi= -C*tm*tm+A*tm
    phi
  }
  //phase at beginning
  def estimatePhaseGivenFreq(freq:Float, portion:Float)={
    val F=AcousticProperty.SINARR_FREQ_MAX-AcousticProperty.SINARR_FREQ_MIN
    val T=AcousticProperty.FMCW_CHIRP_DURATION
    val tm=freq/AcousticProperty.FMCW_WINDOW_DURATION/F*T
    val tT=portion*AcousticProperty.FMCW_WINDOW_RATIO
    val A=tT*2*Math.PI.toFloat*F+2*Math.PI.toFloat*AcousticProperty.SINARR_FREQ_MIN
    val C= Math.PI.toFloat*F/T
    val phi= -C*tm*tm+A*tm
    phi
  }



  // return delta, delta+phact=phest (mod 2pi)
  def phaseDiff(phest:Float, phact:Float)={
    val mul=Math.floor((phest-phact)/Math.PI/2).toFloat
    phest-(mul*Math.PI.toFloat*2+phact)
  }

  def estimateTmGivenPhase(ph:Float, portion:Float)= {
    val F = AcousticProperty.SINARR_FREQ_MAX - AcousticProperty.SINARR_FREQ_MIN
    val T = AcousticProperty.FMCW_CHIRP_DURATION
    val tT=portion*AcousticProperty.FMCW_WINDOW_RATIO
    val A = tT * 2 * Math.PI.toFloat * F + 2 * Math.PI.toFloat * AcousticProperty.SINARR_FREQ_MIN
    val C = Math.PI.toFloat * F / T
    val tm = (A - Math.sqrt(A * A - 4 * C * ph).toFloat) / C / 2
    tm
  }

  //delta+relPh~absPh
  def get2piDelta(relPh:Float, absPh:Float, offset:Float)={
    val diff=absPh-relPh-offset
    val mul=Math.round(diff/2/Math.PI.toFloat)
    val newoffset=absPh-relPh-mul*2*Math.PI.toFloat
    (newoffset, mul*2*Math.PI.toFloat)
  }

  def getAoA(tms:Array[Float]) ={
    val xangles=for ((y, arr)<-verticalPairs) yield{
      val ((x1, _), idx1)=arr(0)
      val ((x2, _), idx2)=arr(1)
      val dist=Math.abs(x1-x2) //in m

      val t1=tms(idx1)
      val t2=tms(idx2)

      val rdist=(t1-t2)*AcousticProperty.SOUND_SPEED // in m
      val angle=Math.asin(rdist/dist)
      angle
    }


    val yangles=for ((x, arr)<-horizontalPairs) yield{
      val ((_, y1), idx1)=arr(0)
      val ((_, y2), idx2)=arr(1)
      val dist=Math.abs(y1-y2) //in m

      val t1=tms(idx1)
      val t2=tms(idx2)

      val rdist=(t1-t2)*AcousticProperty.SOUND_SPEED // in m
      val angle=Math.asin(rdist/dist)
      angle
    }

    (xangles.sum/xangles.size, yangles.sum/yangles.size)
  }



  def getTm(phases:Array[Array[Float]])= {

    def getAvgTm(state:State, phDelta:Float, phase:Array[Float])={
      val N=0
      val tms=for(i<-QUARTER3-N to QUARTER3) yield {
        estimateTmGivenPhase(phDelta + state.phaseOffset + phase(i), i.toFloat/AcousticProperty.FMCW_WINDOW_DURATION_SAMPLE)
      }
      tms.sum/tms.size
    }

    //get the accurate distance estimate
    val tms = phases.zip(states).map { case (phase, state) =>
      if (!state.inited) {
        val (estFreq, initPhase) = estimateFreqInitPhase(phase)
        val estPhase = estimatePhaseGivenFreq(estFreq, 0)
        val phDiff = phaseDiff(estPhase, initPhase)
        state.inited = true
        state.phaseOffset = phDiff
        //println("phdiff "+phDiff)

        val phLast = phase(QUARTER3) - initPhase + estPhase
        val tmLast = estimateTmGivenPhase(phLast, 0.75f)

        //test
        val pht=estimatePhaseGivenTm(tmLast, 0.25f)
        //println("test ph "+(phase(QUARTER1)-initPhase+estPhase)+" "+pht)

        state.lastEndTm = tmLast
        state.lastStartPhase=phase(QUARTER1)-initPhase+estPhase
        state.lastEndPhase=phase(QUARTER3)-initPhase+estPhase

        PosResult(tmLast, 0f)

      } else {
        //first estimate start phase
        val ti=AcousticProperty.FMCW_WINDOW_DURATION*0.5f+AcousticProperty.FMCW_CHIRP_DURATION-AcousticProperty.FMCW_WINDOW_DURATION
        val estTmStart = state.lastEndTm + state.velocity * ti
        val estPhStart = estimatePhaseGivenTm(estTmStart, 0.25f)
        val actPhStart = phase(QUARTER1)
        //println("ph " +(actPhStart+state.phaseOffset)+" "+estPhStart%(Math.PI*2))
        val (newoffset, actPhDelta) = get2piDelta(actPhStart, estPhStart, state.phaseOffset)
        //state.phaseOffset = state.phaseOffset * OFFSET_SMOOTH + newoffset * (1 - OFFSET_SMOOTH)

        val newStartPhase=actPhDelta+state.phaseOffset+phase(QUARTER1)
        val newEndPhase=actPhDelta+state.phaseOffset+phase(QUARTER3)
        val diffstart=newStartPhase-state.lastStartPhase
        val diffend=newEndPhase-state.lastEndPhase

        if(Math.abs(diffstart-diffend)>1.5*Math.PI){
          //error happens, need interpolate
          println("interpolated", newStartPhase, state.lastStartPhase, newEndPhase, state.lastEndPhase)
          phase(QUARTER3)+=(Math.round((diffstart-diffend)/2/Math.PI)*2*Math.PI).toFloat
          phase(QUARTER2)=(phase(QUARTER3)+phase(QUARTER1))/2

        }
        state.lastEndPhase=actPhDelta+state.phaseOffset+phase(QUARTER3)
        state.lastStartPhase=actPhDelta+state.phaseOffset+phase(QUARTER1)

        val estTmLast = estimateTmGivenPhase(actPhDelta + state.phaseOffset + phase(QUARTER3), 0.75f)
        val estVel = (estTmLast - state.lastEndTm) / AcousticProperty.FMCW_CHIRP_DURATION

        state.velocity = estVel
        state.lastEndTm = estTmLast

        val tm=getAvgTm(state, actPhDelta, phase)
        PosResult(tm, estVel)
      }
    }

    tms

  }
  def AoA(tms:Array[Float])={
    //get aoa
    val aoa=getAoA(tms)
    val tm=states.zip(tms).map{
      case (state, t)=>t
    }.sum/tms.length

    (tm, aoa._1, aoa._2)

  }
}
