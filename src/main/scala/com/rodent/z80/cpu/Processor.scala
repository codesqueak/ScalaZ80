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


  //
  //  // 8 bit inc
  //  def inc8(src: Int): Unit = {
  //    if (src == 6) loadFromHL()
  //    val reg = reg8Bit(src)
  //    var v = registers.getReg(reg)
  //    v = (v + 1) & 0xFF
  //    setReg(reg, v)
  //    if (src == 6) loadToHL(reg)
  //  }
  //
  //  // 8 bit dec
  //  def dec8(src: Int): Unit = {
  //    if (src == 6) loadFromHL()
  //    val reg = reg8Bit(src)
  //    var v = registers.getReg(reg)
  //    v = (v - 1) & 0xFF
  //    setReg(reg, v)
  //    if (src == 6) loadToHL(reg)
  //  }
  //
  //  // Utility operations
  //
  //  // Load from memory (HL)
  //  def loadFromHL(): Unit = {
  //    val addr = registers.getReg16(RegNames.H)
  //    registers.setReg(RegNames.M8, memory.getMemory(addr))
  //  }
  //
  //  // Save to memory (HL)
  //  def loadToHL(dstReg: RegName): Unit = {
  //    val addr = registers.getReg16(RegNames.H)
  //    memory.setMemory(addr, registers.getReg(dstReg))
  //  }
}