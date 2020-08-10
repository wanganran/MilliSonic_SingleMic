import config.AcousticProperty
import blocks.SignalGenerator
import blocks.QuadSeparator
import FMCW.{FMCWFilter, FMCWTrack}
import utils.int

object SimulateQuad extends App {
  val clockDiff = 0f //1.0001f //0.1% clock drift
  val initDist = Array(0.7f, 0.7f, 0.7f, 0.7f) //m
  val speed = Array(0.15f,0.05f,0f,-0.1f)//Array(1f, 0.5f, 0f, -0.5f) //m/s
  //val speed=Array(0f,0f,0f,0f)
  val T = 1 //s
  val generater = new SignalGenerator(true)

  val tot_samp=int(T * AcousticProperty.SR)
  val initSignals=Array.range(0,4).map{i=>
    val dT = initDist(
      i
    ) / AcousticProperty.SOUND_SPEED + i * AcousticProperty.FMCW_CHIRP_DURATION / 5
    generater.generateTemplate(0, AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE, dT)
  }.fold(new Array[Float](AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE)) {
      (a, b) =>
        a.indices.map(i => a(i) + b(i)).toArray
    }
  val signals = initSignals ++ Array
    .range(0, 4)
    .map { i =>
      val sampSpeed =
        speed(i) / AcousticProperty.SOUND_SPEED // time per sample
      val clockSpeed = sampSpeed + clockDiff
      val dT = initDist(
        i
      ) / AcousticProperty.SOUND_SPEED + i * AcousticProperty.FMCW_CHIRP_DURATION / 5
      val signal = generater.generateTemplate(
        clockSpeed,
        tot_samp,
        dT
      )
      signal
    }
    .fold(new Array[Float](tot_samp)) {
      (a, b) =>
        a.indices.map(i => a(i) + b(i)).toArray
    }

  val separator = new QuadSeparator(
    generater.getPhaseFunc(),
    AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE / 10
  )
  separator.init(initSignals)
  val fmcwDecodes = Array.range(0,4).map {i=> new FMCWFilter(i*AcousticProperty.FMCW_CHIRP_DURATION/5) }
  val fmcwTrack=new FMCWTrack()
  for (t <- 0 until 18) {

    val sig = signals.slice(
      t * AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE,
      AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE * (t + 1)
    )
    //println(sig.length)
    val sepres = separator.input(
      sig,
      0,
      AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE / 10
    )
    val phases= for (i <- 0 until 4) yield {
      val phaseResult = fmcwDecodes(i).input(sepres(i),clockRatio = clockDiff)
      println(phaseResult.freqEst + "\t" + phaseResult.getDistance())
      phaseResult.phases
    }
    val tms=fmcwTrack.getTm(phases.toArray)

    val gt=Array.range(0,4).map{i=>
      initDist(i)-(t-1)*speed(i)*AcousticProperty.FMCW_CHIRP_DURATION
    }

    println("tms "+tms(0).getDistance()+" "+tms(1).getDistance()+" "+tms(2).getDistance()+" "+tms(3).getDistance())
    println("vel "+tms(0).getVelocity()+" "+tms(1).getVelocity()+" "+tms(2).getVelocity()+" "+tms(3).getVelocity())
    println("gt  "+gt(0)+" "+gt(1)+" "+gt(2)+" "+gt(3))
    println()
  }

}
