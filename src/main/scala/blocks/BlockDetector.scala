package blocks

import utils.Logger

// alpha: the moving average factor
// thres: the min ratio of change of peak_power/(total-peak_power)
// duration: the min duration (# of samples) of non-blocking samples
class BlockDetector (alpha:Float, duration:Int, thres:Float, callback:Boolean=>Unit) {
  var moving_avg:Option[Float]=None
  var last_SNR:Option[Float]=None
  var is_blocked=false
  var non_blocking_cnt=0

  def reset(): Unit ={
    moving_avg=None
    is_blocked=false
    non_blocking_cnt=0
  }

  def get_blocked(sig:Array[Float]):Boolean= {
    //get max i
    var i = 0
    var maxi = 0
    var max = 0f
    var total = 0f
    while (i < sig.length) {
      if (Math.abs(sig(i)) > max) {
        max = Math.abs(sig(i))
        maxi = i
      }
      total += sig(i) * sig(i)
      i += 1
    }
    val snr = max * max / (total - max * max) * sig.length

    if (moving_avg.isEmpty) {
      moving_avg = Some(snr)
      false
    } else if (!is_blocked) {
      val new_avg = moving_avg.get * alpha + (1 - alpha) * snr
      //Logger.writeln("block SNR "+new_avg+" "+snr)
      moving_avg = Some(new_avg)
      if (1 - snr / new_avg > thres) {
        last_SNR = Some(new_avg)
        is_blocked = true
        non_blocking_cnt = 0
        callback(true)
        true
      }
      else false
    } else { // blocked
      moving_avg = Some(moving_avg.get * alpha + (1 - alpha) * snr)
      //Logger.writeln("block SNR "+moving_avg.get+" "+snr)
      if (1 - snr / last_SNR.get < thres) {
        non_blocking_cnt += 1
        if (non_blocking_cnt > duration) {
          // recovered
          is_blocked = false
          non_blocking_cnt = 0
          callback(false)
          false
        } else true
      }
      else {
        non_blocking_cnt = 0
        true
      }
    }
  }
}
