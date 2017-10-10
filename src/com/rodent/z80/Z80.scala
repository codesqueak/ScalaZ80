package com.rodent.z80

import com.rodent.z80.cpu.Processor
import com.rodent.z80.io._

// Simpel test machine config and run
object Z80 extends App {
  println("Z80!")
  val memory = new Memory()
  memory.setMemory(0x21, 0x00, 0x10, 0x76)
  val ports = new Ports()
  val cpu = new Processor(memory, ports);
  cpu.run()
  println("HALT")
  cpu.dump()
}
