package blocks

import java.util.Random

import config.AcousticProperty
import org.jtransforms.fft.FloatFFT_1D
import utils.IQ
import utils.int

class SignalGenerator(FMCW:Boolean) {

  private def generateFMCWPhaseFunc() = {
    val fmin = AcousticProperty.SINARR_FREQ_MIN
    val A = (AcousticProperty.SINARR_FREQ_MAX - AcousticProperty.SINARR_FREQ_MIN) / (AcousticProperty
      .SINARR_DURATION.toFloat / AcousticProperty.SR)
    val sig = new Array[Float](AcousticProperty.SINARR_DURATION)
    for (i <- 0 until sig.length) {
      val t = i.toFloat / AcousticProperty.SR
      sig(i) = Math.cos(2 * Math.PI * fmin * t + 2 * Math.PI * A / 2 * t * t).toFloat
    }
    val fft = new FloatFFT_1D(sig.length)
    fft.realForward(sig)

    (i: Int, f: Float) => {
      val fi = int(f * AcousticProperty.SINARR_DURATION.toFloat / AcousticProperty.SR)

      val real = sig(fi * 2)
      val imag = sig(fi * 2 + 1)
      IQ.fromIQ(real, imag).angle
    }
  }

  private val phaseFunc = generateFMCWPhaseFunc()

  def getPhaseFunc()=phaseFunc


  //ignore GAP
  def generateTemplate(clockRatio:Float, L:Int,dT:Float) = {
    val dest = new Array[Float](L)
    for (i <- 0 until L) {
        val t = i / AcousticProperty.SR.toDouble+i/AcousticProperty.SR.toDouble*clockRatio-dT
        dest(i)=(0 until AcousticProperty.SINARR_FREQ_NUM).map { j =>
          val f = j * AcousticProperty.SINARR_FREQ_GAP + AcousticProperty.SINARR_FREQ_MIN
          Math.cos(2 * Math.PI * f * t + phaseFunc(i, f)).toFloat
        }.sum/AcousticProperty.SINARR_FREQ_NUM
      }
    dest
  }
  def generateTemplate() = {
    Array.range(0, AcousticProperty.SINARR_DURATION).map { k =>
      val t = k / AcousticProperty.SR.toFloat
      (0 until AcousticProperty.SINARR_FREQ_NUM).map { j =>
        val f = j * AcousticProperty.SINARR_FREQ_GAP + AcousticProperty.SINARR_FREQ_MIN
        Math.cos(2 * Math.PI * f * t + phaseFunc(0, f)).toFloat
      }.sum
    }
  }

}
