package blocks


import config.AcousticProperty
import utils.{Conv, int, argmax}

import scala.language.implicitConversions

class QuadSynchronization(template: Array[Float], thres: Float = 0f, init_sample:Int = 0) {
  assert(template.length == AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE)
  private val conv = new Conv(template.length)

  def getTemplateLength = template.length

  private val syncBuffer = new Array[Float](template.length)
  private var ptr = 0
  private var gap = AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE / (AcousticProperty.CONCURRENT_TX + 1)

  private def modr(i: Int) = i % template.length

  def input(sig: Array[Float]): Option[(Float, Int)] = { //returns (snr, offset)
    assert(sig.length == AcousticProperty.SINARR_DURATION)
    Array.copy(sig, 0, syncBuffer, ptr, sig.length)
    ptr += sig.length
    if (ptr == template.length) {
      ptr = 0
      val raw = conv.corrRealRaw(syncBuffer, template,
        int(AcousticProperty.SINARR_FREQ_MIN * template.length / AcousticProperty.SR) - 1,
        int(AcousticProperty.SINARR_FREQ_MAX * template.length / AcousticProperty.SR) + 1)
      val raw2 = Array.concat(raw, raw)
      val peaks = new Array[Int](AcousticProperty.CONCURRENT_TX + 1)
      val peak1 = argmax(raw, raw.length)
      val snr = raw(peak1) / (raw.sum / raw.length)
      if (snr < thres) None else {
        peaks(0) = peak1
        for (i <- 1 until AcousticProperty.CONCURRENT_TX + 1)
          peaks(i) = argmax(raw2.slice(peak1 + (i * 2 - 1) * gap / 2, peak1 + (i * 2 + 1) * gap / 2), gap) + peak1 + (i * 2 - 1) * gap / 2

        //select the min
        val x = argmax(peaks.map(i => -Math.abs(raw2(i))))
        //get the next chunk
        val offset = peaks((x + 1) % (AcousticProperty.CONCURRENT_TX + 1)) - init_sample
        val off = (template.length - offset + AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE) % AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE
        Some((snr, off))
      }
    } else None
  }
}
 