package FMCW

import config.AcousticProperty
import utils.Conv
import utils.argmax
import utils.argmin
import utils.getAngle
import utils.int

import java.io.FileWriter

object FMCWFilter{
  abstract class FMCWResult
  case class PhaseResult(phases:Array[Float], freqEst:Float) extends FMCWResult{
    def getVelocity()={
      //later
      0f
    }
    def getDistance()={
      freqEst/AcousticProperty.FMCW_WINDOW_RATIO*
        AcousticProperty.SOUND_SPEED/
        (AcousticProperty.SINARR_FREQ_MAX-AcousticProperty.SINARR_FREQ_MIN)
    }
  }
}

class FMCWFilter(timeOffset:Float) {

  private val WINDOWSIZE=AcousticProperty.FMCW_WINDOW_DURATION_SAMPLE
  private val WIDTH=WINDOWSIZE/4-1
  private val GAPBEGIN=AcousticProperty.FMCW_WINDOW_OFFSET_SAMPLE
  private val DRIFTLIMIT=10 // samples
  private val convo=new Conv(WINDOWSIZE)
  private val convFFT=new Conv(AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE)

  private val freqMin=int((AcousticProperty.SINARR_FREQ_MIN-1000)*AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE/AcousticProperty.SR)
  private val freqMax=int((AcousticProperty.SINARR_FREQ_MAX+1000)*AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE/AcousticProperty.SR)
  private var id=0


  def setId(id:Int): Unit ={
    this.id=id
  }

  private def generateUpchirpI(dt:Float, clockRatio:Float)={
    val A=(AcousticProperty.SINARR_FREQ_MAX-AcousticProperty.SINARR_FREQ_MIN)/AcousticProperty.FMCW_CHIRP_DURATION
    Array.range(0, AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE).map{i=>
      val t=(i.toDouble/AcousticProperty.SR+dt)*(1+clockRatio.toDouble)
      Math.cos(2*Math.PI*(AcousticProperty.SINARR_FREQ_MIN*t+A/2*t*t)).toFloat
    }
  }

  private def generateUpchirpQ(dt:Float, clockRatio:Float)={
    val A=(AcousticProperty.SINARR_FREQ_MAX-AcousticProperty.SINARR_FREQ_MIN)/AcousticProperty.FMCW_CHIRP_DURATION
    Array.range(0, AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE).map{i=>
      val t=(i.toDouble/AcousticProperty.SR+dt)*(1+clockRatio.toDouble)
      -Math.cos(2*Math.PI*(AcousticProperty.SINARR_FREQ_MIN*t+A/2*t*t)+Math.PI/2).toFloat
    }
  }


  //inputs are all complex
  private def conv(sig:Array[Float], kernel:Array[Float])={
    convo.fftComplexKernel(kernel)
    convo.convComplex(sig, kernel)
  }

  def reset()={
    lastPeak=None
  }

  private var lastPeak:Option[Float]=None

  def updatePeak(peakFreq:Float): Unit ={
    lastPeak=Some(peakFreq)
  }

  private val decodeBuffer=new Array[Float](WINDOWSIZE*2)

  // obsolete
  private def buildWindow(kernel:Array[Float]): Unit ={
    for(i<-0 until WIDTH) {
      val w = 0.53836f - 0.46164f * Math.cos(2 * Math.PI * i / WIDTH).toFloat
      kernel(i * 2) = w
      kernel(i * 2 + 1) = w
      if (i != 0) {
        kernel((WINDOWSIZE - i) * 2) = w
        kernel((WINDOWSIZE - i) * 2 + 1) = w
      }
    }
  }

  private def buildKernel(f:Float)= {
    val kernel = new Array[Float](WINDOWSIZE * 2)

    for (i <- 0 until WIDTH) {
      kernel(i * 2) = Math.cos(2 * Math.PI * i / WINDOWSIZE * f).toFloat / WIDTH
      kernel(i * 2 + 1) = -Math.sin(2 * Math.PI * i / WINDOWSIZE * f).toFloat / WIDTH
      if (i != 0) {
        kernel((WINDOWSIZE - i) * 2) = Math.cos(2 * Math.PI * i / WINDOWSIZE * f).toFloat / WIDTH
        kernel((WINDOWSIZE - i) * 2 + 1) = -Math.sin(2 * Math.PI * i / WINDOWSIZE * f).toFloat / WIDTH
      }
    }
    kernel
  }

  private var sampOffset=0

  private var outputted=0

  private def modx(x:Int, m:Int):Int=if(x>=0) x%m else modx(x+m,m)
  //return (raw_phases, raw_freq, distance (in s), velocity (relative to speed of sound))
  private def doFMCWFilter(chirpI:Array[Float], chirpQ:Array[Float], realSig:Array[Float], begin:Int)= {
    convFFT.realBpf(realSig, freqMin, freqMax)

    val first=lastPeak.isEmpty

    for (i <- 0 until WINDOWSIZE) {
      decodeBuffer(i*2) = chirpI(i+begin) * realSig(modx(i+begin-sampOffset, AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE))
      decodeBuffer(i*2+1) = chirpQ(i+begin) * realSig(modx(i+begin-sampOffset, AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE))
    }
    if (first) {
      val copyBuffer = new Array[Float](decodeBuffer.length)
      Array.copy(decodeBuffer, 0, copyBuffer, 0, decodeBuffer.length)
      val freqs = convo.fftComplexKernel(copyBuffer)
      for (i <- 1 until WINDOWSIZE)
        freqs(i) = freqs(i*2) * freqs(i*2) + freqs(i*2+1) * freqs(i*2+1)


      //test
      val fileout = new FileWriter("data/fmcwdata"+id.toString()+"." + outputted.toString() + ".txt")
      for (j <- 0 until WINDOWSIZE)
        fileout.write(freqs(j).toString() + "\t")
      fileout.close()
      outputted += 1

      /*for(i<-0 until WINDOWSIZE)
        print(freqs(i)+"\t") 
      println()
      */

      val freqMax = argmax(freqs, WINDOWSIZE)
      lastPeak = Some(freqMax)
      //println(freqMax)
    }

    //test
    val copyBuffer = new Array[Float](decodeBuffer.length)
    Array.copy(decodeBuffer, 0, copyBuffer, 0, decodeBuffer.length)
    val freqs = convo.fftComplexKernel(copyBuffer)
    for (i <- 1 until WINDOWSIZE)
      freqs(i) = freqs(i*2) * freqs(i*2) + freqs(i*2+1) * freqs(i*2+1)
    val fileout = new FileWriter("data/fmcwdata"+id.toString()+"." + outputted.toString() + ".txt")
    for (j <- 0 until WINDOWSIZE)
      fileout.write(freqs(j).toString() + "\t")
    fileout.close()
    outputted += 1



    val kernel = buildKernel(lastPeak.get)
    convo.fftComplexKernel(kernel)


    convo.convComplex(decodeBuffer, kernel)

    var totalDiff = 0f
    var lastRawphase=0f
    val phaseBuffer=new Array[Float](WINDOWSIZE)

    //get lowest and check phase
    val LOWNEIGHBOR=WINDOWSIZE/20

    for(i<-WIDTH until WINDOWSIZE-WIDTH){
      phaseBuffer(i)=decodeBuffer(i*2)*decodeBuffer(i*2)+decodeBuffer(i*2+1)*decodeBuffer(i*2+1)
    }


    for (i <- WIDTH until WINDOWSIZE - WIDTH) {
      val rawPhase = getAngle(decodeBuffer(i*2), decodeBuffer(i*2+1))
      if (i != WIDTH) {
        val rawdiff = rawPhase-lastRawphase
        if (rawdiff > Math.PI) {
          val actdiff = rawdiff - 2 * Math.PI.toFloat
          totalDiff += rawdiff - 2 * Math.PI.toFloat
          phaseBuffer(i) = phaseBuffer(i - 1) + actdiff
        }
        else if (rawdiff < -Math.PI) {
          val actdiff = rawdiff + 2 * Math.PI.toFloat
          totalDiff += rawdiff + 2 * Math.PI.toFloat
          phaseBuffer(i) = phaseBuffer(i - 1) + actdiff
        }
        else {
          totalDiff += rawdiff
          phaseBuffer(i) = phaseBuffer(i - 1) + rawdiff
        }
        lastRawphase=rawPhase
      } else {
        lastRawphase=rawPhase
        phaseBuffer(i)=rawPhase
      }
    }

    val peak=totalDiff/(WINDOWSIZE-2*WIDTH-1)*WINDOWSIZE/Math.PI.toFloat/2
    (phaseBuffer, peak)
  }

  var time=System.currentTimeMillis()
  
  var currentDt=0d
  var first=true

  //realSig is disposable
  //return (phase diff array, avg freq, dist, vel)
  def input(realSig:Array[Float], clockRatio:Double)={
    if(first){
      currentDt=timeOffset*clockRatio

    }
    val upchirpI=generateUpchirpI(currentDt.toFloat, clockRatio.toFloat)
    val upchirpQ=generateUpchirpQ(currentDt.toFloat, clockRatio.toFloat)

    if(first) {
      val (_, prevFreqUp)=doFMCWFilter(upchirpI, upchirpQ, realSig, GAPBEGIN)
      //println("first "+prevFreqUp)
      lastPeak=Some(prevFreqUp)
    }

    val (phases, freqUp)=doFMCWFilter(upchirpI, upchirpQ, realSig, GAPBEGIN)
    //println("second "+freqUp)
    lastPeak=Some(freqUp)

    first=false

    currentDt+=clockRatio*AcousticProperty.FMCW_CHIRP_DURATION
    if(currentDt>DRIFTLIMIT.toFloat/AcousticProperty.SR){
      currentDt-=DRIFTLIMIT.toFloat/AcousticProperty.SR
      sampOffset+=DRIFTLIMIT
    } else if(currentDt< -DRIFTLIMIT.toFloat/AcousticProperty.SR){
      currentDt+=DRIFTLIMIT.toFloat/AcousticProperty.SR
      sampOffset-=DRIFTLIMIT
    }
    println("currentDt "+currentDt+ " " + (DRIFTLIMIT.toFloat/AcousticProperty.SR)+ " "+phases(WIDTH))
    FMCWFilter.PhaseResult(phases, freqUp)
  }
  def skip(clockRatio:Double): Unit ={
    currentDt+=clockRatio*AcousticProperty.FMCW_CHIRP_DURATION
    if(currentDt>DRIFTLIMIT.toFloat/AcousticProperty.SR){
      currentDt-=DRIFTLIMIT.toFloat/AcousticProperty.SR
      sampOffset+=DRIFTLIMIT
    } else if(currentDt< -DRIFTLIMIT.toFloat/AcousticProperty.SR){
      currentDt+=DRIFTLIMIT.toFloat/AcousticProperty.SR
      sampOffset-=DRIFTLIMIT
    }
  }

  //return drift amount in s
  def getTotalDriftTime()={
    currentDt+sampOffset/AcousticProperty.SR
  }
}
