package blocks

import config.AcousticProperty
import utils.{IQ, int}
import org.jtransforms.fft.FloatFFT_1D
import utils.Conv

import java.io.{FileOutputStream, FileWriter}

class QuadSeparator(phaseShiftFunc: (Int, Float) => Float, transition:Int) {
  private val freqs = Array.range(0, AcousticProperty.SINARR_FREQ_NUM).map { i =>
    AcousticProperty.SINARR_FREQ_MIN + i * AcousticProperty.SINARR_FREQ_GAP
  }

  private val conv = new Conv(AcousticProperty.SINARR_DURATION)
  private val quarter = AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE / 5

  def coswind(i: Float) = (Math.cos(i * Math.PI).toFloat + 1) / 2

  val window = Array.range(0, AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE).map { i =>
    //window centered at 1/8 of the range and 1/4 of width
    if (i > transition / 2 && i < quarter - transition / 2) 1
    else if (i > quarter + transition / 2 && i < AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE - transition / 2) 0
    else if (i >= quarter - transition / 2 && i <= quarter + transition / 2)
      coswind((i - quarter + transition / 2).toFloat / transition)
    else
      coswind((transition / 2 - i).toFloat / transition)
  }
  val fft = new FloatFFT_1D(AcousticProperty.SINARR_DURATION)

  var lastSig: Array[Float] = null

  def init(sig: Array[Float]): Unit = {
    lastSig = sig
  }

  var outputted=0
  //return quad float sig array
  def input(sig: Array[Float], id: Int, offset: Int): Array[Array[Float]] = {

    val newSig = new Array[Float](AcousticProperty.SINARR_DURATION)
    val result = for (i <- 0 until 4) yield {
      val currSig = if (i == 0) lastSig else lastSig.slice(quarter * i, lastSig.length) ++ sig.slice(0, quarter * i)
      val sigPhase = conv.fftReal(currSig)
      freqs.foreach { f =>
        val af = int(f / AcousticProperty.SINARR_FREQ_GAP)
        val real = sigPhase(af * 2)
        val imag = sigPhase(af * 2 + 1)
        val delayed = IQ.fromIQ(real, imag).shift(-phaseShiftFunc(id, f))
        sigPhase(af * 2) = delayed.getI
        sigPhase(af * 2 + 1) = delayed.getQ
      }
      fft.realInverse(sigPhase, false)

      //test output
      if(outputted<4) {
        outputted += 1
        val fileout = new FileWriter("data/testdata" + i.toString() + ".txt")
        for (j <- 0 until newSig.length)
          fileout.write(sigPhase(j).toString() + "\t")
        fileout.close()
      }
      //end test output

      for (j <- 0 until newSig.length) {
        newSig(j) = sigPhase(j) * window((j + offset + window.length) % window.length)
      }
      fft.realForward(newSig)

      for (af <- 0 until AcousticProperty.SINARR_DURATION / 2) {
        if (af >= int(AcousticProperty.SINARR_FREQ_MIN / AcousticProperty.SINARR_FREQ_GAP) && af <= int(AcousticProperty.SINARR_FREQ_MAX / AcousticProperty.SINARR_FREQ_GAP)) {
          val real = newSig(af * 2)
          val imag = newSig(af * 2 + 1)
          val f = af * AcousticProperty.SINARR_FREQ_GAP
          val delayed = IQ.fromIQ(real, imag).shift(phaseShiftFunc(id, f))
          newSig(af * 2) = delayed.getI
          newSig(af * 2 + 1) = delayed.getQ
        }
        else {
          newSig(af * 2) = 0
          newSig(af * 2 + 1) = 0
        }
      }
      fft.realInverse(newSig, false)
      newSig.clone()
    }
    lastSig=sig
    result.toArray
  }
}
