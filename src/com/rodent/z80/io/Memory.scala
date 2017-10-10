package com.rodent.z80.io

class Memory {
  // Set memory to HALT
  private val ram = Array.fill[Int](65536)(0x76)

  // load a pattern into memory
  def setMemory(b: Int*): Unit = {
    var addr = 0
    for (elem <- b) {
      setMemory(addr, elem)
      addr += 1
    }
  }

  def getMemory(addr: Int): Int = ram(addr)

  def setMemory(addr: Int, byteVal: Int): Unit = ram(addr) = byteVal

}