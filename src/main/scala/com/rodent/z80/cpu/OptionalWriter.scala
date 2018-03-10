package com.rodent.z80.cpu

import com.rodent.z80.CPUZ._
import com.rodent.z80.func.Utils
import com.rodent.z80.io.Memory

// Process additional memory writes where required

trait OptionalWriter {
  private val atHL = 6
  val memory: Memory


  def write(registers: Registers): Registers = {
    val r = writeOperations(registers)
    //
    if ((r.internalRegisters.dd) && (r.internalRegisters.inst != 0xDD))
      return r.copy(internalRegisters = r.internalRegisters.copy(dd = false))
    //
    if ((r.internalRegisters.fd) && (r.internalRegisters.inst != 0xFD))
      return r.copy(internalRegisters = r.internalRegisters.copy(fd = false))
    //
    if ((r.internalRegisters.cb) && (r.internalRegisters.inst != 0xCB))
      return r.copy(internalRegisters = r.internalRegisters.copy(cb = false))
    //
    if ((r.internalRegisters.ed) && (r.internalRegisters.inst != 0xED))
      return r.copy(internalRegisters = r.internalRegisters.copy(ed = false))
    //
    if ((r.internalRegisters.ddcb) && (r.internalRegisters.inst != 0xCB))
      return r.copy(internalRegisters = r.internalRegisters.copy(ddcb = false))
    //
    if ((r.internalRegisters.fdcb) && (r.internalRegisters.inst != 0xCB))
      r.copy(internalRegisters = r.internalRegisters.copy(fdcb = false))
    else
      r
  }


  // Deal with additional memory writes, both 8 and 16 bit for extended instructions
  private def writeOperations(r: Registers): Registers = {
    if (r.internalRegisters.dd || r.internalRegisters.fd) {
      r.internalRegisters.x match {
        case 0 if r.internalRegisters.inst == 0x22 => save16toNN(r) // LD (nn), IXIY
        case 0 if (r.internalRegisters.z == 4) && (r.internalRegisters.y == atHL) => save8toIXIY(r) // inc (ixiy)
        case 0 if (r.internalRegisters.z == 5) && (r.internalRegisters.y == atHL) => save8toIXIY(r) // dec (ixiy)
        case 0 if (r.internalRegisters.z == 6) && (r.internalRegisters.y == atHL) => save8toIXIY(r) // ld (ixiy),n
        //
        case 1 if r.internalRegisters.y == atHL => save8toIXIYcalcAddr(r)
        //
        case _ => r
      }
    }
    else {
      if (r.internalRegisters.ed) {
        r.internalRegisters.inst match {
          case 0xA0 => save8toM16(r)
          case 0xA8 => save8toM16(r)
          case 0xB0 => save8toM16(r)
          case 0xB8 => save8toM16(r)
          //
          case _ => r
        }
      }
      else {
        r.internalRegisters.x match {
          case 0 if r.internalRegisters.inst == 0x02 => save8toBC(r) // LD (BC), A
          case 0 if r.internalRegisters.inst == 0x12 => save8toDE(r) // LD (DE), A
          case 0 if r.internalRegisters.inst == 0x22 => save16toNN(r) // LD (nn), HL
          case 0 if r.internalRegisters.inst == 0x32 => save8toNN(r) // LD (nn), A

          case 0 if (r.internalRegisters.z == 4) && (r.internalRegisters.y == atHL) => save8toHL(r) // inc (hl)
          case 0 if (r.internalRegisters.z == 5) && (r.internalRegisters.y == atHL) => save8toHL(r) // dec (hl)
          case 0 if (r.internalRegisters.z == 6) && (r.internalRegisters.y == atHL) => save8toHL(r) // ld (hl),n
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
  }

  // Save byte to (HL)
  private def save8toHL(registers: Registers): Registers = {
    var addr = registers.getReg16(RegNames.H)
    memory.setMemory(addr, registers.regFile1.m8)
    println("writing " + Utils.toHex8(registers.regFile1.m8) + " to " + Utils.toHex16(addr))
    registers
  }

  // Re-save byte at (IXIY+dd), e.g. inc/dec
  private def save8toIXIY(registers: Registers): Registers = {
    // address is held in m16
    var addr = registers.getReg16(RegNames.M16)
    memory.setMemory(addr, registers.regFile1.m8)
    registers
  }

  // Save byte to (IXIY+dd)
  private def save8toIXIYcalcAddr(registers: Registers): Registers = {
    val v = registers.getReg(RegNames.M8)
    val r = loadImmediate8(registers) // get dd offset
    var addr = r.getReg(RegNames.M8)
    if (addr > 127) addr = addr - 256
    if (r.internalRegisters.dd)
      addr = (r.getReg16(RegNames.IX) + addr).limit16
    else
      addr = (r.getReg16(RegNames.IY) + addr).limit16
    // address if held in m16
    memory.setMemory(addr, registers.regFile1.m8)
    r
  }

  // Save byte to (M16)
  private def save8toM16(registers: Registers): Registers = {
    println("writing " + Utils.toHex8(registers.regFile1.m8) + " to " + Utils.toHex16(registers.getReg16(RegNames.M16)))
    memory.setMemory(registers.getReg16(RegNames.M16), registers.regFile1.m8)
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
  private def save8toNN(registers: Registers): Registers

  = {
    var r = loadImmediate16(registers)
    memory.setMemory(r.regFile1.m16, r.regFile1.m8)
    r
  }

  // Save word to (HL)
  private def save16toNN(registers: Registers): Registers

  = {
    val v = registers.regFile1.m16
    var r = loadImmediate16(registers)
    memory.setMemory16(r.regFile1.m16, v)
    r
  }

  // Load 8 bit value following instruction
  private def loadImmediate8(registers: Registers): Registers

  = {
    var addr = registers.getPC
    val v = memory.getMemory(addr)
    val rf1 = registers.regFile1.copy(m8 = v)
    val cr = registers.controlRegisters.copy(pc = addr.inc16)
    registers.copy(controlRegisters = cr, regFile1 = rf1)
  }

  // Load 16 bit value following instruction
  private def loadImmediate16(registers: Registers): Registers

  = {
    var addr = registers.getPC
    val lsb = memory.getMemory(addr)
    addr = addr.inc16
    val v = (memory.getMemory(addr) << 8) + lsb
    val rf1 = registers.setBaseReg16(RegNames.M16, v)
    val cr = registers.controlRegisters.copy(pc = addr.inc16)
    registers.copy(controlRegisters = cr, regFile1 = rf1)
  }
}

