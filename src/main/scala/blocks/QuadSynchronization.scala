package blocks


import config.AcousticProperty
import utils.{Conv, int, argmax}

import scala.language.implicitConversions

class QuadSynchronization(template: Array[Float], thres: Float = 0f) {
  assert(template.length == AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE)
  private val conv = new Conv(template.length)

  def getTemplateLength = template.length

  private val syncBuffer = new Array[Float](template.length)
  private var ptr = 0
  private var gap = AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE / 4

  private def modr(i: Int) = i % template.length

  def input(sigs: Array[Array[Float]]): Option[(Int, Int)] = { //returns (id, offset)
    assert(sigs(0).length == AcousticProperty.SINARR_DURATION)
    Array.copy(sigs(0), 0, syncBuffer, ptr, sigs(0).length)
    ptr += sigs(0).length
    if (ptr == template.length) {
      ptr = 0
      val raw = conv.corrRealRaw(syncBuffer, template,
        int(AcousticProperty.SINARR_FREQ_MIN * template.length / AcousticProperty.SR) - 1,
        int(AcousticProperty.SINARR_FREQ_MAX * template.length / AcousticProperty.SR) + 1)
      val raw2 = Array.concat(raw, raw)
      val peaks = new Array[Int](4)
      val peak1 = argmax(raw, raw.length)
      if (raw(peak1) / (raw.sum / raw.length) < thres) None else {
        peaks(0) = peak1
        peaks(1) = argmax(raw2.slice(peak1 + gap / 2, peak1 + 3 * gap / 2), gap) + peak1 + gap / 2
        peaks(2) = argmax(raw2.slice(peak1 + 3 * gap / 2, peak1 + 5 * gap / 2), gap) + peak1 + 3 * gap / 2
        peaks(3) = argmax(raw2.slice(peak1 + 5 * gap / 2, peak1 + 7 * gap / 2), gap) + peak1 + 5 * gap / 2

        //select the min
        val x = argmax(peaks.map(i => -Math.abs(raw2(i))))
        //get the next chunk
        val offset = peaks((x + 1) % 4) - gap / 2
        val off = (template.length - offset) % AcousticProperty.FMCW_CHIRP_DURATION_SAMPLE
        Some((0, off))
      }
    } else None
  }
}
 