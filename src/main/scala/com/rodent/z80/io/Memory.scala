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

  def setMemory16(addr: Int, wordVal: Int): Unit = {
    ram(addr) = wordVal & 0x00FF
    ram((addr + 1) & 0xFFFF) = (wordVal & 0xFF00) >>> 8
  }

  def getMemory16(addr: Int): Int = {
    ram(addr) + ram((addr + 1) & 0xFFFF) << 8
  }

  def push(addr: Int, wordVal: Int): Unit = {
    ram((addr - 1) & 0xFFFF) = (wordVal & 0xFF00) >>> 8
    ram((addr - 2) & 0xFFFF) = wordVal & 0x00ff
  }

  def pop(addr: Int): Int = {
    ram(addr + (ram((addr + 1) & 0xFFFF) << 8))
  }
}
