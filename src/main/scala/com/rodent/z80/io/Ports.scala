package com.rodent.z80.io

import scala.runtime.RichInt

// Z90 I/O ports
class Ports() {

  def getPort(addr: Int): Int = 0

  def setPort(addr: Int, byteVal: Int): Unit = {
    if (0 == addr)
      if (byteVal < 32) println
      else if (byteVal > 128)
        println("weird")
      else
        print(byteVal.toChar)
    else
      println(addr + " : " + new RichInt(byteVal).toHexString)
  }

}
