package com.rodent.z80.cpu

import com.rodent.z80.CPU._
import com.rodent.z80.io.Memory

import scala.language.implicitConversions

trait ALU {

  val reg8Bit = Map(0 -> RegNames.B, 1 -> RegNames.C, 2 -> RegNames.D, 3 -> RegNames.E, 4 -> RegNames.H, 5 -> RegNames.L, 6 -> RegNames.M8, 7 -> RegNames.A)
  val reg16Bit = Map(0 -> RegNames.B, 1 -> RegNames.D, 2 -> RegNames.H, 3 -> RegNames.SP)

  val memory: Memory

  def execute(registers: Registers): Registers = {
    // http://www.z80.info/decoding.htm
    // http://www.z80.info/z80oplist.txt
    // http://www.z80.info/z80sflag.htm
    var r = registers
    if (r.internalRegisters.inst == 0x76) {
      // HALT processing (Interrupt flip flops etc)
    }
    else {
      registers.internalRegisters.x match {
        case 0 => general0(r)
        case 1 => load8(r)
        case 2 => println("todo 1")
        case 3 => println("todo 2")
      }
    }
    r
  }

  // Instructions 0x00 -> 0x3F
  // Instructions 0x00 -> 0x3F
  // Instructions 0x00 -> 0x3F

  // instruction prefix 0
  private def general0(registers: Registers): Registers = {
    var r = registers
    r.internalRegisters.z match {
      case 0 => jmpRelative(r)
      case 1 => ldadd16(r)
      case 2 => indirectLoad(r)
      case 3 => incDec16(r)
      case 4 => inc8(r)
      case 5 => dec8(r)
      case 6 => ldImmediate8(r)
      case 7 => variousBlock0(r)
    }
  }

  // various block 0 jumps
  private def jmpRelative(r: Registers): Registers = {
    r.internalRegisters.y match {
      case 0 => r // nop
      case 1 => // ex af,af'
        val af1 = r.getReg16(RegNames.A)
        val af2 = r.getAltReg16(RegNames.A)
        val rf1 = r.setBaseReg16(RegNames.A, af2)
        val rf2 = r.setAltBaseReg16(RegNames.A, af1)
        r.copy(regFile1 = rf1, regFile2 = rf2)
      case 2 => // djnz d
        val b = r.getReg(RegNames.B).dec8 // oddly doesn't impact flags ...
        r.copy(regFile1 = r.setBaseReg(RegNames.B, b))
        if (0 != b) jr(r)
      case 3 => jr(r) // jr d
      case 4 if r.isNZ => jr(r) // jr nz d
      case 5 if r.isZ => jr(r) // jr z d
      case 6 if r.isNC => jr(r) // jr nc d
      case 7 if r.isC => jr(r) // jr c d
    }
    r
  }

  // Indirect loading
  private def indirectLoad(r: Registers): Registers = {
    var rf1: BaseRegisters = null
    if (0 == r.internalRegisters.q)
      r.internalRegisters.p match {
        case 0 => setMemory8fromA(r) // LD (BC), A
        case 1 => setMemory8fromA(r) // LD (DE), A
        case 2 => setMemory16fromHL(r) // LD (nn), HL
        case 3 => setMemory8fromA(r) // LD (nn), A
      }
    else
      r.internalRegisters.p match {
        case 0 => setAfromMemory8(r) // LD A, (BC)
        case 1 => setAfromMemory8(r) // LD A, (DE)
        case 2 => setHLfromMemory16(r) // LD HL, (nn)
        case 3 => setAfromMemory8(r) // LD A, (nn)
      }
  }

  // various block 0, 8 bit math ops
  private def variousBlock0(r: Registers): Registers = {
    r.internalRegisters.y match {
      case 0 => r // rlca
      case 1 => r // rrca
      case 2 => r // rla
      case 3 => r // rra
      case 4 => r // daa
      case 5 => r // cpl
      case 6 => r // scf
      case 7 => r // ccf
    }
  }

  // ld r,nn
  private def ldImmediate8(r: Registers): Registers = {
    val regName = reg8Bit(r.p)
    r.copy(regFile1 = r.setBaseReg(regName, r.getReg(RegNames.M8)))
  }

  // inc / dec rr
  private def incDec16(r: Registers): Registers = {
    var cr = r.controlRegisters
    var rf1 = r.regFile1
    val regName = reg16Bit(r.p)
    var v = r.getReg16(regName)
    v = if (0 == r.q) v.inc16 else v.dec16
    if (regName == RegNames.SP)
      cr = r.setControlReg(regName, v)
    else
      rf1 = r.setBaseReg16(regName, v)
    r.copy(controlRegisters = cr, regFile1 = rf1)
  }

  // inc r
  private def inc8(r: Registers): Registers = {
    val regName = reg8Bit(r.p)
    val v = r.getReg(regName).inc8
    r.copy(regFile1 = r.setBaseReg(regName, v))
  }

  // dec r
  private def dec8(r: Registers): Registers = {
    val regName = reg8Bit(r.p)
    val v = r.getReg(regName).dec8
    r.copy(regFile1 = r.setBaseReg(regName, v))
  }

  // LD / ADD 16
  private def ldadd16(registers: Registers): Registers = {
    var r = registers.regFile1
    var cr = registers.controlRegisters
    val v = r.m16
    if (registers.internalRegisters.q == 0) {
      // LD rr,(mm)
      val lsb = v & 0x00FF
      val msb = (v & 0xFF00) >>> 8
      registers.internalRegisters.p match {
        case 0 => r = r.copy(b = msb, c = lsb)
        case 1 => r = r.copy(d = msb, e = lsb)
        case 2 => r = r.copy(h = msb, l = lsb)
        case 3 => cr = cr.copy(sp = v)
      }
    } else {
      // ADD HL,rr
      val v = registers.getReg16(RegNames.H) + registers.getReg16(reg16Bit(registers.internalRegisters.p));
      val lsb = v & 0x00FF
      val msb = (v & 0xFF00) >>> 8
      registers.internalRegisters.p match {
        case 0 => r = r.copy(b = msb, c = lsb)
        case 1 => r = r.copy(d = msb, e = lsb)
        case 2 => r = r.copy(h = msb, l = lsb)
        case 3 => cr = cr.copy(sp = v)
      }
    }
    registers.copy(regFile1 = r, controlRegisters = cr)
  }

  // Instructions 0x40 -> 0x7F
  // Instructions 0x40 -> 0x7F
  // Instructions 0x40 -> 0x7F

  // standard 8 bit ld src,dst instrucitons
  private def load8(registers: Registers): Registers = {
    val srcReg = registers.internalRegisters.z
    val v = registers.getReg(reg8Bit(srcReg))
    // 0 1 2 3 4 5 6 7
    // B C D E H L M A
    var r = registers.regFile1
    registers.internalRegisters.y match {
      case 0 => r = r.copy(b = v)
      case 1 => r = r.copy(c = v)
      case 2 => r = r.copy(d = v)
      case 3 => r = r.copy(e = v)
      case 4 => r = r.copy(h = v)
      case 5 => r = r.copy(l = v)
      case 6 => r = r.copy(m8 = v)
      case 7 => r = r.copy(a = v)
    }
    registers.copy(regFile1 = r)
  }

  // helpers - helpers - helpers
  // helpers - helpers - helpers
  // helpers - helpers - helpers

  // relative jump
  private def jr(r: Registers): Registers = {
    var offset = r.getReg(RegNames.M8)
    if (offset > 0x007F) offset = offset - 0x0100
    val pc = (r.getPC + offset) & 0xFFFF
    r.copy(controlRegisters = r.setPC(pc))
  }

  // absolute jump
  private def jp(r: Registers): Registers = {
    r.copy(controlRegisters = r.setPC(r.getReg(RegNames.M16)))
  }

  private def setMemory8fromA(r: Registers): Registers = {
    r.copy(regFile1 = r.setBaseReg(RegNames.M8, r.getReg(RegNames.A)))
  }

  private def setAfromMemory8(r: Registers): Registers = {
    r.copy(regFile1 = r.setBaseReg(RegNames.A, r.getReg(RegNames.M8)))
  }

  private def setMemory16fromHL(r: Registers): Registers = {
    r.copy(regFile1 = r.setBaseReg16(RegNames.M16, r.getReg16(RegNames.H)))
  }

  private def setHLfromMemory16(r: Registers): Registers = {
    r.copy(regFile1 = r.setBaseReg16(RegNames.H, r.getReg16(RegNames.M16)))
  }

}

