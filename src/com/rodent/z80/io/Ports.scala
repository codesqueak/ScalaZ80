package com.rodent.z80.io

import scala.runtime.RichInt

// Z90 I/O ports
class Ports() {

  def getPort(addr: Int): Int = 0

  def setPort(addr: Int, byteVal: Int): Unit = {
    println(addr + " : " + new RichInt(byteVal).toHexString)
  }

}
