package com.rodent.z80.cpu

import com.rodent.z80.CPU.RegNames
import com.rodent.z80.io.Memory

// Process additional memory reads where required

trait OptionalReader {
  private val atHL = 6
  val memory: Memory

  // Deal with additional memory reads, both 8 and 16 bit for extended instructions
  def read(r: Registers): Registers = {
    r.internalRegisters.x match {
      case 0 if r.internalRegisters.inst == 0x21 => loadImmediate16(r)
      case 0 if (r.internalRegisters.z == 4) && (r.internalRegisters.p == atHL) => load8atHL(r)
      case 0 if (r.internalRegisters.z == 5) && (r.internalRegisters.p == atHL) => load8atHL(r)
      case 0 => r
      //
      case 1 if r.internalRegisters.z == atHL => load8atHL(r)
      case 2 => r
      case 3 => r
      //
      case _ => r
    }
  }

  // Load byte at (HL)
  private def load8atHL(registers: Registers): Registers = {
    var m8 = memory.getMemory(registers.getReg16(RegNames.H))
    val br = registers.regFile1.copy(m8 = m8)
    registers.copy(regFile1 = br)
  }

  // Load 16 bit value following instruction
  private def loadImmediate16(registers: Registers): Registers = {
    var addr = registers.getPC
    val lsb = memory.getMemory(addr)
    addr = (addr + 1) & 0xFFFF
    val v = (memory.getMemory(addr) << 8) + lsb
    val ir = registers.internalRegisters.copy(m16 = v)
    val cr = registers.controlRegisters.copy(pc = (addr + 1) & 0xFFFF)
    registers.copy(controlRegisters = cr, internalRegisters = ir)
  }
}
