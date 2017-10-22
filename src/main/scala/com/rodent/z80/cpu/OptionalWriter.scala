package com.rodent.z80.cpu

import com.rodent.z80.CPU.RegNames
import com.rodent.z80.io.Memory

// Process additional memory writes where required

trait OptionalWriter {
  private val atHL = 6
  val memory: Memory

  // Deal with additional memory writes, both 8 and 16 bit for extended instructions
  def write(r: Registers): Registers = {
    r.internalRegisters.x match {
      case 0 if (r.internalRegisters.z == 4) && (r.internalRegisters.p == atHL) => save8toHL(r)
      case 0 if (r.internalRegisters.z == 5) && (r.internalRegisters.p == atHL) => save8toHL(r)
      case 1 if r.internalRegisters.y == 6 => save8toHL(r)
      case 2 => r
      case 3 => r
      case _ => r
    }
  }

  // Save byte at (HL)
  private def save8toHL(registers: Registers): Registers = {
    if (registers.internalRegisters.z != 6) {
      var addr = registers.getReg16(RegNames.H)
      memory.setMemory(addr, registers.regFile1.m8)
    }
    registers
  }

}

