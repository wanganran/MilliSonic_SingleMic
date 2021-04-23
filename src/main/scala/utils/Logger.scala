package utils

import java.io.FileWriter

object Logger {
  var tag=""
  var fileWriter:FileWriter=null
  def setDest(file:String): Unit ={
    fileWriter = if (file != null) new FileWriter(file) else null
  }
  def setTag(tag:String): Unit ={
    this.tag=tag
  }

  def writeln(str: String): Unit = {
    val time=System.currentTimeMillis()
    val msg="[" + tag + "]\t" + time + "\t" + str + "\n"
    if (fileWriter == null) print(msg)
    else {
      fileWriter.write(msg)
      fileWriter.flush()
    }
  }
}
