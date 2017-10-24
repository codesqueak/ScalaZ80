package com.rodent.z80.cpu

import com.rodent.z80.CPU._
import com.rodent.z80.io.Memory

import scala.language.implicitConversions

trait ALU {
  // flags register bits
  val s = 0x80
  val z = 0x40
  val f5 = 0x20
  val h = 0x10
  val f3 = 0x08
  val pv = 0x04
  val n = 0x02
  val c = 0x01

  val reg8Bit = Map(0 -> RegNames.B, 1 -> RegNames.C, 2 -> RegNames.D, 3 -> RegNames.E, 4 -> RegNames.H, 5 -> RegNames.L, 6 -> RegNames.M8, 7 -> RegNames.A)
  val reg16Bit = Map(0 -> RegNames.B, 1 -> RegNames.D, 2 -> RegNames.H, 3 -> RegNames.SP)

  val memory: Memory

  def execute(registers: Registers): Registers = {
    // http://www.z80.info/decoding.htm
    // http://www.z80.info/z80oplist.txt
    var r = registers
    if (r.internalRegisters.inst == 0x76) {
      // HALT processing (Interrupt flip flops etc)
    }
    else {
      registers.internalRegisters.x match {
        case 0 => r = general0(r)
        case 1 => r = load8(r)
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
      case 1 => r // ex af,af'
      case 2 => r // djnz d
      case 3 => r // jr d
      case 4 => r // jr nz d
      case 5 => r // jr z d
      case 6 => r // jr nc d
      case 7 => r // jr c d
    }
  }

  // Indirect loading
  private def indirectLoad(r: Registers): Registers = {
    if (0 == r.internalRegisters.q)
      r.internalRegisters.p match {
        case 0 => r // LD (BC), A
        case 1 => r // LD (DE), A
        case 2 => r // LD (nn), HL
        case 3 => r // LD (nn), A
      }
    else
      r.internalRegisters.p match {
        case 0 => r // LD A, (BC)
        case 1 => r // LD A, (DE)
        case 2 => r // LD HL, (nn)
        case 3 => r // LD A, (nn)
      }
    r
  }

  // various block 0 8 bit math ops
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
    val v = registers.internalRegisters.m16
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
}

