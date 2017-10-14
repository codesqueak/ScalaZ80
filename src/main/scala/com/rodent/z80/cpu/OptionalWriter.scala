package com.rodent.z80.cpu

import com.rodent.z80.CPU.RegNames
import com.rodent.z80.io.Memory

// Process additional memory writes where required

trait OptionalWriter {
  val memory: Memory

  // Deal with additional memory writes, both 8 and 16 bit for extended instructions
  def write(registers: Registers): Registers = {
    registers.internalRegisters.x match {
      case 0 =>
      case 1 => if (registers.internalRegisters.y == 6) save8toHL(registers)
      case 2 =>
      case 3 =>
    }
    registers
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

