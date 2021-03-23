package config

import utils.{Position, int}

/**
  * Created by anwang on 7/10/2017.
  */
object AcousticProperty {
  val SR = 50000

  val ARRAY_BUFFER_SIZE=100
  val DRIVER_PREHEAT=10

  val SOUND_SPEED = 343.0f //m/s

  val SINARR_FREQ_MIN = 18000f
  val SINARR_FREQ_MAX = 22000f

  val SINARR_FREQ_GAP = 20f


  val SINARR_FREQ_NUM = int((SINARR_FREQ_MAX - SINARR_FREQ_MIN) / SINARR_FREQ_GAP) + 1
  val SINARR_DURATION = int(SR / SINARR_FREQ_GAP)

  //speaker board sync freq
  val SPEAKER_CLOCK_INTERVAL=1f //once per second

  //default drift
  val DEFAULT_DRIFT = -0.000012619 //1.000000381*0.999987

  //motion detection
  val CALIBRATION_DURATION=1f
  val RESTABLE_DURATION=5f

  //drift correction
  val SYNC_ALPHA=0.99f
  val SYNC_BETA=1f

  //FMCW cannot run with duration_gap
  val FMCW_CHIRP_DURATION_SAMPLE = SINARR_DURATION
  val FMCW_CHIRP_DURATION = FMCW_CHIRP_DURATION_SAMPLE/SR.toFloat

  val TRANSITION = FMCW_CHIRP_DURATION_SAMPLE/8

  val CONCURRENT_TX=4

  val FMCW_WINDOW_RATIO=0.8f
  val FMCW_WINDOW_OFFSET_RATIO=0.1f

  val FMCW_WINDOW_DURATION_SAMPLE = int(FMCW_CHIRP_DURATION_SAMPLE * FMCW_WINDOW_RATIO)
  val FMCW_WINDOW_OFFSET_SAMPLE = int(FMCW_CHIRP_DURATION_SAMPLE * FMCW_WINDOW_OFFSET_RATIO)
  val FMCW_WINDOW_DURATION = FMCW_CHIRP_DURATION*FMCW_WINDOW_RATIO
  val FMCW_WINDOW_OFFSET = FMCW_CHIRP_DURATION*FMCW_WINDOW_RATIO

  val FMCW_CHANNELS_CONFIG=Array((0f,0f), (6e-2f, 0f), (0f, 5.35e-2f), (6e-2f, 5.35e-2f))
}
