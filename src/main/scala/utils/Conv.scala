package utils

import org.jtransforms.fft.FloatFFT_1D

/**
  * Created by anranw on 6/27/18.
  */
class Conv(length: Int) {
  private val fft = new FloatFFT_1D(length)

  def fftReal(data: Array[Float]) = {
    val result = data.clone()
    fft.realForward(result)
    result
  }

  def fftComplexKernel(kernel: Array[Float]) = {
    fft.complexForward(kernel)
    kernel
  }


  //real signal, freq in [0, length/2)
  def realBpf(realSig: Array[Float], freqStart: Int = 0, freqEnd: Int = -1) = {
    fft.realForward(realSig)
    for (i <- 0 until freqStart) {
      realSig(i * 2) = 0
      realSig(i * 2 + 1) = 0
    }
    for (i <- (if (freqEnd < 0) freqEnd + length / 2 else freqEnd) + 1 until length / 2) {
      realSig(i * 2) = 0
      realSig(i * 2 + 1) = 0
    }
    fft.realInverse(realSig, false)
  }

  //complex signal, freq in [0, length)
  def complexBpf(complexSig: Array[Float], freqStart: Int = 0, freqEnd: Int = -1) = {
    fft.complexForward(complexSig)
    for (i <- 0 until freqStart) {
      complexSig(i * 2) = 0
      complexSig(i * 2 + 1) = 0
    }

    for (i <- (if (freqEnd < 0) freqEnd + length else freqEnd) + 1 until length) {
      complexSig(i * 2) = 0
      complexSig(i * 2 + 1) = 0
    }
    fft.complexInverse(complexSig, false)
    complexSig
  }

  // inclusive: [freqStart, freqaEnd]
  // inplace for data
  def convComplex(data: Array[Float], kernel: Array[Float], freqStart: Int = 0, freqEnd: Int = -1) = {
    fft.complexForward(data)

    for (i <- 0 until freqStart) {
      data(i * 2) = 0
      data(i * 2 + 1) = 0
    }

    for (i <- freqStart to (freqEnd + length) % length) {
      val t = data(i * 2) * kernel(i * 2) + data(i * 2 + 1) * kernel(i * 2 + 1)
      data(i * 2 + 1) = -data(i * 2) * kernel(i * 2 + 1) + data(i * 2 + 1) * kernel(i * 2)
      data(i * 2) = t
    }

    for (i <- (if (freqEnd < 0) freqEnd + length else freqEnd) + 1 until length) {
      data(i * 2) = 0
      data(i * 2 + 1) = 0
    }

    fft.complexInverse(data, false)
    data
  }

  def corrReal(data: Array[Float], template: Array[Float], freqStart: Int = 0, freqEnd: Int = -1) =
    corrRealWithSNR(data, template, freqStart, freqEnd)._1

  //not in-place
  def corrRealRaw(data: Array[Float], template: Array[Float], freqStart: Int = 0, freqEnd: Int = -1) = {
    val newData = new Array[Float](data.length * 2)
    val newTemplate = new Array[Float](data.length * 2)
    for (i <- 0 until data.length) {
      newData(i * 2) = data(i)/data.length
    }
    for (i <- 0 until template.length) {
      newTemplate(i * 2) = template(i)/template.length
    }
    fftComplexKernel(newTemplate)
    convComplex(newData, newTemplate, freqStart, freqEnd)
    for (i <- 0 until data.length)
      newData(i) = newData(i * 2) * newData(i * 2) + newData(i * 2 + 1) * newData(i * 2 + 1)

    newData.slice(0, data.length)
  }
  
  def corrRealWithSNR(data: Array[Float], template: Array[Float], freqStart: Int = 0, freqEnd: Int = -1) = {
    val newData=corrRealRaw(data, template, freqStart, freqEnd)

    val pos = argmax(newData, data.length)
    val mean = (newData.sum - newData(pos)) / (data.length - 1)
    (pos, newData(pos) / mean)
  }
}
