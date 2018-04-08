package com.rodent.z80.cpu

import com.rodent.z80.io._

// Z80 Processor core

class Processor(m: Memory, p: Ports, var registers: Registers) extends Fetch with Decode with OptionalReader with ALU with OptionalWriter {

  val memory: Memory = m
  val ports: Ports = p

  def run(): Unit = {
    var continue: Boolean = true
    // Pipeline fetch -> decode -> read(opt) -> execute -> write(opt)
    while (!registers.halt) {
      registers = fetch(registers)
      registers = decode(registers)
      registers = read(registers)
      registers = execute(registers)
      registers = write(registers)
    }
    println("Halt")
  }

  def run(addr: Int): Unit = {
    registers = registers.copy(control = registers.setPC(addr))
    run()
  }


  def dump(): Unit = {
    registers.dump()
  }


}