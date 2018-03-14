package com.rodent.z80.cpu

import com.rodent.z80.CPUZ._
import com.rodent.z80.io.Memory

// Process additional memory reads where required

trait OptionalReader {
  private val atHL = 6
  val memory: Memory

  // Deal with additional memory reads, both 8 and 16 bit for extended instructions
  def read(r: Registers): Registers = {
    if (r.internalRegisters.ed) {
      r.internalRegisters.x match {
        case 1 if (r.internalRegisters.z == 3) && (r.internalRegisters.q == 0) => loadImmediate16(r) // LD (nn), rp[p]
        case 1 if (r.internalRegisters.z == 3) && (r.internalRegisters.q == 1) =>
          load16atNN(r) // LD (nn), rp[p]
        //
        case _ => r.internalRegisters.inst match {
          case 0xA0 => load8atHL(r)
          case 0xA8 => load8atHL(r)
          case 0xB0 => load8atHL(r)
          case 0xB8 => load8atHL(r)
          //
          case _ => r
        }
      }
    }
    else {
      if (r.internalRegisters.cb) {
        if (r.internalRegisters.z == atHL)
          load8atHL(r) // bit n,(hl)
        else
          r
      } else {
        if (r.internalRegisters.ddcb_load || r.internalRegisters.fdcb_load) {
          load8atIXIY(r)
        }
        else {
          if (r.internalRegisters.dd || r.internalRegisters.fd) {
            if (r.internalRegisters.inst == 0xCB)
              return r
            r.internalRegisters.x match {
              case 0 if r.internalRegisters.inst == 0x21 => loadImmediate16(r) // LD IXIY,NN
              case 0 if r.internalRegisters.inst == 0x2A => load16atNN(r) // LD IXIY, (nn)
              case 0 if (r.internalRegisters.z == 4) && (r.internalRegisters.y == atHL) => load8atIXIY(r) // inc (ixiy)
              case 0 if (r.internalRegisters.z == 5) && (r.internalRegisters.y == atHL) => load8atIXIY(r) // dec (ixiy)
              case 0 if r.internalRegisters.z == 6 => loadImmediate8IXIY(r) // ld (ixiy+dd),n
              //
              case 1 if r.internalRegisters.z == atHL => load8atIXIY(r)
              //
              case 2 if r.internalRegisters.z == atHL => load8atIXIY(r)
              //
              case _ => r
            }
          }
          else {
            r.internalRegisters.x match {
              case 0 if r.internalRegisters.inst == 0x01 => loadImmediate16(r) // LD BC,NN
              case 0 if r.internalRegisters.inst == 0x11 => loadImmediate16(r) // LD DE,NN
              case 0 if r.internalRegisters.inst == 0x21 => loadImmediate16(r) // LD HL,NN
              case 0 if r.internalRegisters.inst == 0x31 => loadImmediate16(r) // LD SP,NN
              case 0 if r.internalRegisters.inst == 0x0A => load8atBC(r) // LD A, (BC)
              case 0 if r.internalRegisters.inst == 0x1A => load8atDE(r) // LD A, (DE)
              case 0 if r.internalRegisters.inst == 0x2A => load16atNN(r) // LD HL, (nn)
              case 0 if r.internalRegisters.inst == 0x3A => load8atNN(r) // LD A, (nn)

              case 0 if (r.internalRegisters.z == 0) && (r.internalRegisters.y > 1) => loadImmediate8(r) //jr
              case 0 if (r.internalRegisters.z == 4) && (r.internalRegisters.y == atHL) => load8atHL(r) // inc (hl)
              case 0 if (r.internalRegisters.z == 5) && (r.internalRegisters.y == atHL) => load8atHL(r) // dec (hl)
              case 0 if r.internalRegisters.z == 6 => loadImmediate8(r) // ld r,n

              case 0 => r
              //
              case 1 if r.internalRegisters.z == atHL => load8atHL(r)
              case 1 => r
              //
              case 2 if r.internalRegisters.z == atHL => load8atHL(r)
              case 2 => r
              //
              case 3 if r.internalRegisters.z == 2 => loadImmediate16(r) // jp cc nn
              case 3 if (r.internalRegisters.z == 3) && (r.internalRegisters.y == 0) => loadImmediate16(r) // jp nn
              case 3 if (r.internalRegisters.z == 3) && (r.internalRegisters.y == 2) => loadImmediate8(r) // out(n),a
              case 3 if (r.internalRegisters.z == 3) && (r.internalRegisters.y == 3) => loadImmediate8(r) // in a,(n)
              case 3 if r.internalRegisters.z == 4 => loadImmediate16(r) // call cc
              case 3 if (r.internalRegisters.z == 5) && (r.internalRegisters.q == 1) && (r.internalRegisters.p == 0) => loadImmediate16(r) // call nn
              case 3 if r.internalRegisters.z == 6 => loadImmediate8(r) // alu nn
              case 3 => r
              //
              case _ => r
            }
          }
        }
      }
    }
  }

  // Load byte at (HL)
  private def load8atHL(registers: Registers): Registers = {
    val rf1 = registers.regFile1.copy(m8 = memory.getMemory(registers.getReg16(RegNames.H)))
    registers.copy(regFile1 = rf1)
  }

  // Load byte at (IXIY+dd)
  // save calculated address into m16
  private def load8atIXIY(registers: Registers): Registers = {
    val r = loadImmediate8(registers) // get dd offset
    var addr = r.getReg(RegNames.M8)
    if (addr > 127) addr = addr - 256
    if (r.internalRegisters.dd)
      addr = (r.getReg16(RegNames.IX) + addr).limit16
    else
      addr = (r.getReg16(RegNames.IY) + addr).limit16
    val rf1 = r.regFile1.copy(m8 = memory.getMemory(addr), m16 = addr)
    r.copy(regFile1 = rf1)
  }

  // Load byte at (BC)
  private def load8atBC(registers: Registers): Registers = {
    val br = registers.regFile1.copy(m8 = memory.getMemory(registers.getReg16(RegNames.B)))
    registers.copy(regFile1 = br)
  }

  // Load byte at (DE)
  private def load8atDE(registers: Registers): Registers = {
    val br = registers.regFile1.copy(m8 = memory.getMemory(registers.getReg16(RegNames.D)))
    registers.copy(regFile1 = br)
  }

  // Load byte at (NN)
  private def load8atNN(registers: Registers): Registers = {
    var r = loadImmediate16(registers)
    r.copy(regFile1 = r.setBaseReg(RegNames.M8, memory.getMemory(r.regFile1.m16)))
  }

  // Load word at (NN)
  private def load16atNN(registers: Registers): Registers = {
    var r = loadImmediate16(registers)
    var addr = r.regFile1.m16
    r.copy(regFile1 = r.setBaseReg(RegNames.M16, memory.getMemory16(addr)))
  }

  // Load 8 bit value following instruction
  private def loadImmediate8(registers: Registers): Registers = {
    var addr = registers.getPC
    val v = memory.getMemory(addr)
    val rf1 = registers.regFile1.copy(m8 = v)
    val cr = registers.controlRegisters.copy(pc = addr.inc16)
    registers.copy(controlRegisters = cr, regFile1 = rf1)
  }

  // Load 8 bit value following instruction
  // save calculated target address into m16
  private def loadImmediate8IXIY(registers: Registers): Registers = {
    val r = loadImmediate8(registers) // get dd offset
    var addr = r.getReg(RegNames.M8)
    if (addr > 127) addr = addr - 256
    if (r.internalRegisters.dd)
      addr = (r.getReg16(RegNames.IX) + addr).limit16
    else
      addr = (r.getReg16(RegNames.IY) + addr).limit16
    r.copy(regFile1 = r.regFile1.copy(m16 = addr))
    loadImmediate8(r) // get n
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
