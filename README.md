# MilliSonic_SingleMic
MilliSonic implementation using multiple speakers and a single microphone

## To run using Serial input
* Install Sbt, and run `sbt` in command line; then type `run serial COMx` where `COMx` is the Serial port name. Choose `SerialMain` if needed.
* Or, install IntellijIdea, import and run directly.

## To run using file input (the file may be the recordings using https://github.com/uw-x/bp-calibration)
* `run file [filepath]`
The estimated locations will be output to the stdout. 
