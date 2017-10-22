package com.rodent.z80.cpu

import com.rodent.z80.io._

// Z80 Processor core

class Processor(m: Memory, p: Ports, var registers: Registers) extends Fetch with Decode with OptionalReader with ALU with OptionalWriter {

  val memory: Memory = m
  val ports: Ports = p

  def run(): Unit = {
    var continue: Boolean = true
    // Pipeline fetch -> decode -> read(opt) -> execute -> write(opt)
    while (registers.getInst != 0x76) {
      registers = fetch(registers)
      registers = decode(registers)
      registers = read(registers)
      registers = execute(registers)
      registers = write(registers)
    }
  }

  def dump(): Unit = {
    registers.dump()
  }

}