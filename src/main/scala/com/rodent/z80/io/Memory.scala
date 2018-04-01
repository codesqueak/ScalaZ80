package com.rodent.z80.io

class Memory {
  // Set memory to HALT
  private val ram = Array.fill[Int](65536)(0x76)

  // load a pattern into memory
  def setMemory(b: Array[Int]): Int = {
    setMemory(0, b)
  }

  // load a pattern into memory
  def setMemory(addr: Int, b: Array[Int]): Int = {
    var a = addr
    for (elem <- b) {
      setMemory(a, elem)
      a += 1
    }
    a
  }

  def getMemory(addr: Int): Int = {
    //   println("read " + Utils.toHex8(ram(addr)) + " @ " + Utils.toHex16(addr))
    ram(addr)
  }

  def setMemory(addr: Int, byteVal: Int): Unit = {
    //         println("write " + Utils.toHex8(byteVal) + " @ " + Utils.toHex16(addr))
    ram(addr) = byteVal
  }

  def setMemory16(addr: Int, wordVal: Int): Unit = {
    setMemory(addr, wordVal & 0x00FF)
    setMemory((addr + 1) & 0xFFFF, (wordVal & 0xFF00) >>> 8)
  }

  def getMemory16(addr: Int): Int = {
    ram(addr) + (ram((addr + 1) & 0xFFFF) << 8)
  }

  def push(addr: Int, wordVal: Int): Unit = {
    //    println("push: " + Utils.toHex16(wordVal) + " @ " + Utils.toHex16(addr))
    setMemory((addr - 1) & 0xFFFF, (wordVal & 0xFF00) >>> 8)
    setMemory((addr - 2) & 0xFFFF, wordVal & 0x00ff)
  }

  def pop(addr: Int): Int = {
    val v = ram(addr) + (ram((addr + 1) & 0xFFFF) << 8)
    //  println("pop: " + Utils.toHex16(v) + " @ " + Utils.toHex16(addr))
    v
  }
}
