package com.rodent.z80.cpu

import com.rodent.z80.CPUZ._
import com.rodent.z80.io.Memory

// Process additional memory writes where required

trait OptionalWriter {
  private val atHL = 6
  val memory: Memory

  // Deal with additional memory writes, both 8 and 16 bit for extended instructions
  def write(r: Registers): Registers = {
    if (r.internalRegisters.cb) {
      if (r.internalRegisters.z == atHL)
        save8toHL(r) // bit n,(hl)
      else
        r
    }
    else {
      r.internalRegisters.x match {
        case 0 if r.internalRegisters.inst == 0x02 => save8toBC(r) // LD (BC), A
        case 0 if r.internalRegisters.inst == 0x12 => save8toDE(r) // LD (DE), A
        case 0 if r.internalRegisters.inst == 0x22 => save16toHL(r) // LD (nn), HL
        case 0 if r.internalRegisters.inst == 0x32 => save8toNN(r) // LD (nn), A

        case 0 if (r.internalRegisters.z == 4) && (r.internalRegisters.y == atHL) => save8toHL(r)
        case 0 if (r.internalRegisters.z == 5) && (r.internalRegisters.y == atHL) => save8toHL(r)
        case 0 if (r.internalRegisters.z == 6) && (r.internalRegisters.y == atHL) => save8toHL(r)
        //
        case 1 if r.internalRegisters.y == 6 => save8toHL(r)
        //
        case 2 => r
        //
        case 3 => r
        //
        case _ => r
      }
    }
  }

  // Save byte to (HL)
  private def save8toHL(registers: Registers): Registers = {
    var addr = registers.getReg16(RegNames.H)
    memory.setMemory(addr, registers.regFile1.m8)
    registers
  }

  // Save byte to (BC)
  private def save8toBC(registers: Registers): Registers = {
    memory.setMemory(registers.getReg16(RegNames.B), registers.regFile1.m8)
    registers
  }

  // Save byte to (DE)
  private def save8toDE(registers: Registers): Registers = {
    memory.setMemory(registers.getReg16(RegNames.D), registers.regFile1.m8)
    registers
  }

  // Save byte to (NN)
  private def save8toNN(registers: Registers): Registers = {
    var r = loadImmediate16(registers)
    memory.setMemory(r.regFile1.m16, r.regFile1.m8)
    r
  }

  // Save word to (HL)
  private def save16toHL(registers: Registers): Registers = {
    val v = registers.regFile1.m16
    var r = loadImmediate16(registers)
    memory.setMemory16(r.regFile1.m16, v)
    r
  }

  // Load 8 bit value following instruction
  private def loadImmediate8(registers: Registers): Registers = {
    var addr = registers.getPC
    val v = memory.getMemory(addr)
    val rf1 = registers.regFile1.copy(m8 = v)
    val cr = registers.controlRegisters.copy(pc = addr.inc16)
    registers.copy(controlRegisters = cr, regFile1 = rf1)
  }

  // Load 16 bit value following instruction
  private def loadImmediate16(registers: Registers): Registers = {
    var addr = registers.getPC
    val lsb = memory.getMemory(addr)
    addr = addr.inc16
    val v = (memory.getMemory(addr) << 8) + lsb
    val rf1 = registers.setBaseReg16(RegNames.M16, v)
    val cr = registers.controlRegisters.copy(pc = addr.inc16)
    registers.copy(controlRegisters = cr, regFile1 = rf1)
  }
}

