package com.rodent.z80

import com.rodent.z80.cpu.{Processor, Registers}
import com.rodent.z80.io._

// Simple test machine config and run
object Z80 extends App {
  println("Z80!")
  val memory = new Memory()
  memory.setMemory(Array(0x7E,
    0x21, 0x00, 0x10, // ld hl, 1000
    0x29, // add hl,hl
    0x23, // inc hl
    0x76))
  val ports = new Ports()
  val cpu = new Processor(memory, ports, Registers())
  cpu.run()
  println("HALT")
  cpu.dump()
}
