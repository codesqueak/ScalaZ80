package com.rodent.z80.cpu

import com.rodent.z80.CPUZ._
import com.rodent.z80.func.Utils
import com.rodent.z80.io.{Memory, Ports}

import scala.language.implicitConversions

trait ALU {

  val reg8Bit = Map(0 -> RegNames.B, 1 -> RegNames.C, 2 -> RegNames.D, 3 -> RegNames.E, 4 -> RegNames.H, 5 -> RegNames.L, 6 -> RegNames.DATA8, 7 -> RegNames.A)
  val reg8BitIXIY = Map(0 -> RegNames.B, 1 -> RegNames.C, 2 -> RegNames.D, 3 -> RegNames.E, 4 -> RegNames.IXIYH, 5 -> RegNames.IXIYL, 6 -> RegNames.DATA8, 7 -> RegNames.A)
  val reg16Bit = Map(0 -> RegNames.B, 1 -> RegNames.D, 2 -> RegNames.H, 3 -> RegNames.SP)
  val reg16Bit2 = Map(0 -> RegNames.B, 1 -> RegNames.D, 2 -> RegNames.H, 3 -> RegNames.A)

  val nibbleParity = Array(0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4)

  val testBit = Array(0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80)

  val resetBit: Array[Int] = testBit.map(x => x ^ 0xFF)

  val FLAG_S = 0x80
  val FLAG_Z = 0x40
  val FLAG_F5 = 0x20
  val FLAG_H = 0x10
  val FLAG_F3 = 0x08
  val FLAG_PV = 0x04
  val FLAG_N = 0x02
  val FLAG_C = 0x01

  val memory: Memory
  val ports: Ports

  val SET_FLAG = Option(true)
  val RESET_FLAG = Option(false)

  def execute(r: Registers): Registers = {
    // http://www.z80.info/decoding.htm
    // http://www.z80.info/z80oplist.txt
    // http://www.z80.info/z80sflag.htm
    // http://clrhome.org/table/
    // https://raine.1emulation.com/archive/dev/z80-documented.pdf
    //
    if ((r.internal.inst == 0x76) & r.single) {
      // HALT processing (Interrupt flip flops etc)
      r.copy(internal = r.setHalt)
    }
    else {
      if (r.internal.single)
      // single byte
        r.internal.x match {
          case 0 => general0(r)
          case 1 => load8(r)
          case 2 => general8BitALU(r)
          case 3 => general3(r)
        }
      else if (r.internal.ed) {
        executeED(r).copy(internal = r.internal.copy(single = true, ed = false))
      }
      else if (r.internal.cb)
        if (r.dd || r.fd)
          executeDDFDCB(r).copy(internal = r.internal.copy(single = true, cb = false, dd = false, fd = false))
        else
          executeCB(r).copy(internal = r.internal.copy(single = true, cb = false))
      else if (r.dd || r.fd) {
        val regs = executeDDFD(r)
        if (regs.internal.cb)
          regs
        else
          regs.copy(internal = regs.internal.copy(single = true, dd = false, fd = false))
      }
      else
        throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst : " + Utils.toHex8(r.internal.inst))
    }
  }

  // Instructions 0x00 -> 0x3F
  // Instructions 0x00 -> 0x3F
  // Instructions 0x00 -> 0x3F

  private def general0(r: Registers): Registers = {
    r.internal.z match {
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

  // Instructions 0x80 -> 0xBF
  // Instructions 0x80 -> 0xBF
  // Instructions 0x80 -> 0xBF

  private def general8BitALU(r: Registers): Registers = {
    r.internal.y match {
      case 0 => add8(r)
      case 1 => adc8(r)
      case 2 => sub8(r)
      case 3 => sbc8(r)
      case 4 => and8(r)
      case 5 => xor8(r)
      case 6 => or8(r)
      case 7 => cp8(r)
    }
  }

  // Instructions 0xC0 -> 0xFF
  // Instructions 0xC0 -> 0xFF
  // Instructions 0xC0 -> 0xFF

  private def general3(r: Registers): Registers = {
    r.internal.z match {
      case 0 => retcc(r)
      case 1 => variousExPop(r)
      case 2 => jpcc(r)
      case 3 => variousJpExInOut(r)
      case 4 => callcc(r)
      case 5 => variousPushCallSpecial(r)
      case 6 => general8BitALU(r)
      case 7 => rst(r)
    }
  }

  // various block 3, ex, pop etc
  private def variousExPop(r: Registers): Registers = {
    if (0 == r.internal.q) {
      // pop
      val rf1 = r.setBaseReg16(reg16Bit2(r.internal.p), memory.pop(r.getSP))
      val cr = r.setSP(r.getSP.inc16.inc16)
      r.copy(regFile1 = rf1, control = cr)
    }
    else
      r.internal.p match {
        case 0 => ret(r)
        case 1 => r.copy(regFile1 = r.regFile2, regFile2 = r.regFile1) // exx
        case 2 => r.copy(control = r.setPC(r.getReg16(RegNames.H))) // jp HL
        case 3 => r.copy(control = r.setSP(r.getReg16(RegNames.H))) // ld sp,hl
      }
  }

  // various block 3, jp,ex,in,out
  private def variousJpExInOut(r: Registers): Registers = {
    r.internal.y match {
      case 0 => jp(r) // jp nn
      case 1 =>
        r.copy(internal = r.internal.copy(single = false, cb = true))
      case 2 => ports.setPort(r.regFile1.data8.get, r.getA)
        r // out
      case 3 =>
        r.copy(regFile1 = r.regFile1.copy(a = 0, f = 0)) // need to add flag effects
      case 4 => // ex (sp),hl
        val temp = memory.pop(r.getSP)
        memory.push(r.getSP.inc16.inc16, r.getReg16(RegNames.H))
        r.copy(regFile1 = r.setBaseReg16(RegNames.H, temp))
      case 5 => r.copy(regFile1 = r.regFile1.copy(h = r.regFile1.d, l = r.regFile1.e, d = r.regFile1.h, e = r.regFile1.l)) // ex hl,de
      case 6 => r // DI
      case 7 => r // EI
    }
  }

  // various block 3, pushCallSpecial
  private def variousPushCallSpecial(r: Registers): Registers = {
    if (0 == r.internal.q) {
      // push
      memory.push(r.getSP, r.getReg16(reg16Bit2(r.internal.p)))
      val cr = r.setSP(r.getSP.dec16.dec16)
      r.copy(control = cr)
    }
    else
      r.internal.p match {
        case 0 => call(r)
        case 1 => r.copy(internal = r.internal.copy(single = false, dd = true))
        case 2 => r.copy(internal = r.internal.copy(single = false, ed = true))
        case 3 => r.copy(internal = r.internal.copy(single = false, fd = true))
      }
  }

  // various block 0 jumps
  private def jmpRelative(r: Registers): Registers = {
    r.internal.y match {
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
        if (0 != b) jr(r) else r
      case 3 => jr(r) // jr d
      case 4 if r.isNZ => jr(r) // jr nz d
      case 4 => r
      case 5 if r.isZ => jr(r) // jr z d
      case 5 => r
      case 6 if r.isNC => jr(r) // jr nc d
      case 6 => r
      case 7 if r.isC => jr(r) // jr c d
      case 7 => r
    }
  }

  // Indirect loading
  private def indirectLoad(r: Registers): Registers = {
    if (0 == r.internal.q)
      r.internal.p match {
        case 0 => setMemory8fromA(r, r.getReg16(RegNames.B)) // LD (BC), A
        case 1 => setMemory8fromA(r, r.getReg16(RegNames.D)) // LD (DE), A
        case 2 if r.internal.dd => setMemory16fromIX(r, r.getReg16(RegNames.DATA16)) // LD (nn), IX
        case 2 if r.internal.fd => setMemory16fromIY(r, r.getReg16(RegNames.DATA16)) // LD (nn), IY
        case 2 => setMemory16fromHL(r, r.getReg16(RegNames.DATA16)) // LD (nn), HL
        case 3 => setMemory8fromA(r, r.getReg16(RegNames.DATA16)) // LD (nn), A
      }
    else
      r.internal.p match {
        case 0 => setAfromMemory8(r) // LD A, (BC)
        case 1 => setAfromMemory8(r) // LD A, (DE)
        case 2 if r.internal.dd => setIXfromMemory16(r) // LD IX, (nn)
        case 2 if r.internal.fd => setIYfromMemory16(r) // LD IY, (nn)
        case 2 => setHLfromMemory16(r) // LD HL, (nn)
        case 3 => setAfromMemory8(r) // LD A, (nn)
      }
  }

  // various block 0, 8 bit math ops
  private def variousBlock0(r: Registers): Registers = {
    r.internal.y match {
      case 0 => rlca(r) // rlca
      case 1 => rrca(r) // rrca
      case 2 => rla(r) // rla
      case 3 => rra(r) // rra
      case 4 => daa(r) // daa
      case 5 => cpl(r) // cpl
      case 6 => scf(r) // scf
      case 7 => ccf(r) // ccf
    }
  }

  // rlca
  private def rlca(r: Registers): Registers = {
    val c = (r.getA & FLAG_S) != 0
    var v = (r.getA << 1).limit8
    if (c) v = v | FLAG_C
    r.copy(regFile1 = r.setResultA(v, cf = Option(c), f5f = v.f5, f3f = v.f3, hf = RESET_FLAG, nf = RESET_FLAG))
  }

  // rrca
  private def rrca(r: Registers): Registers = {
    val c = (r.getA & FLAG_C) != 0
    var v = r.getA >>> 1
    if (c) v = v | FLAG_S else v = v & 0x7f
    r.copy(regFile1 = r.setResultA(v, cf = Option(c), f5f = v.f5, f3f = v.f3, hf = RESET_FLAG, nf = RESET_FLAG))
  }

  // rla
  private def rla(r: Registers): Registers = {
    val c = (r.getA & FLAG_S) != 0
    var a = (r.getA << 1).limit8
    if (r.isC) a = a | FLAG_C
    r.copy(regFile1 = r.setResultA(a, cf = Option(c), f5f = a.f5, f3f = a.f3, hf = RESET_FLAG, nf = RESET_FLAG))
  }

  // rra
  private def rra(r: Registers): Registers = {
    val c = (r.getA & FLAG_C) != 0
    var a = r.getA >>> 1
    if (r.isC) a = a | FLAG_S
    r.copy(regFile1 = r.setResultA(a, cf = Option(c), f5f = a.f5, f3f = a.f3, hf = RESET_FLAG, nf = RESET_FLAG))
  }

  // cpl
  private def cpl(r: Registers): Registers = {
    val a = r.getA ^ 0xFF
    r.copy(regFile1 = r.setResultA(a, f5f = a.f5, f3f = a.f3, hf = SET_FLAG, nf = SET_FLAG))
  }

  // scf
  private def scf(r: Registers): Registers = {
    val a = r.getA
    r.copy(regFile1 = r.setFlags(f5f = a.f5, f3f = a.f3, hf = RESET_FLAG, nf = RESET_FLAG, cf = SET_FLAG))
  }

  // ccf
  private def ccf(r: Registers): Registers = {
    val a = r.getA
    r.copy(regFile1 = r.setFlags(f5f = a.f5, f3f = a.f3, hf = Option(r.isC), nf = RESET_FLAG, cf = Option(!r.isC)))
  }

  // DAA is weird, can't find Zilog algorithm so using +0110 if Nibble>9 algorithm.
  private def daa(r: Registers): Registers = {
    val ans = r.getA
    var incr = 0
    var carry = r.isC
    if (r.isH || ((ans & 0x0f) > 0x09)) {
      incr = 0x06
    }
    if (r.isC || (ans > 0x9f) || ((ans > 0x8f) && ((ans & 0x0f) > 0x09))) {
      incr |= 0x60
    }
    if (ans > 0x99) {
      carry = true
    }
    var flags = r.getReg(RegNames.F)
    var local_reg_A = r.getA
    if (r.isN) {
      // sub
      flags = if (getHalfCarryFlagSub(local_reg_A, incr)) flags | 0x10 else flags & 0xef // h
      flags = if (getOverflowFlagSub(local_reg_A, incr)) flags | 0x04 else flags & 0xfb // pv
      local_reg_A = local_reg_A - incr
      //
      flags = if ((local_reg_A & 0x0080) != 0) flags | 0x80 else flags & 0x7f // s
      flags = if ((local_reg_A & 0xff00) != 0) flags | 0x01 else flags & 0xfe // c
      local_reg_A = local_reg_A & 0x00ff
      flags = if (local_reg_A == 0) flags | 0x40 else flags & 0xbf // z
      flags = flags | 0x02 // n
      flags = if (local_reg_A.f3.get) flags | 0x08 else flags & 0xf7 // f3
      flags = if (local_reg_A.f5.get) flags | 0x20 else flags & 0xdf // f5
    } else {
      // add
      flags = if (getHalfCarryFlagAdd(local_reg_A, incr)) flags | 0x10 else flags & 0xef
      flags = if (getOverflowFlagAdd(local_reg_A, incr)) flags | 0x04 else flags & 0xfb
      local_reg_A = local_reg_A + incr
      flags = if ((local_reg_A & 0x0080) != 0) flags | 0x80 else flags & 0x7f // s
      flags = if ((local_reg_A & 0xff00) != 0) flags | 0x01 else flags & 0xfe // c
      local_reg_A = local_reg_A & 0x00ff
      flags = if (local_reg_A == 0) flags | 0x40 else flags & 0xbf
      flags = flags & 0xfd // n
      flags = if (local_reg_A.f3.get) flags | 0x08 else flags & 0xf7
      flags = if (local_reg_A.f5.get) flags | 0x20 else flags & 0xdf
    }
    flags = if (carry) flags | 0x01 else flags & 0xfe
    flags = if (getParityFlag(local_reg_A)) flags | 0x04 else flags & 0xfb
    r.copy(regFile1 = r.regFile1.copy(a = local_reg_A, f = flags))
  }

  // ld r,nn
  private def ldImmediate8(r: Registers): Registers = {
    val regName = reg8Bit(r.internal.y)
    if (regName == RegNames.DATA8)
      r.copy(regFile1 = r.regFile1.copy(data16 = None, wz = Option(r.getReg16(RegNames.H))))
    else
      r.copy(regFile1 = r.setBaseReg(regName, r.getReg(RegNames.DATA8)))
  }

  // inc / dec rr
  private def incDec16(r: Registers): Registers = {
    val regName = reg16Bit(r.p)
    var v = if (r.dd && regName == RegNames.H)
      r.getReg16(RegNames.IX)
    else if (r.fd && regName == RegNames.H)
      r.getReg16(RegNames.IY)
    else
      r.getReg16(regName)
    //
    v = if (0 == r.q) v.inc16 else v.dec16
    //
    if (regName == RegNames.SP)
      r.copy(control = r.setControlReg(regName, v))
    else if (r.internal.dd)
      r.copy(index = r.setIndexReg(RegNames.IX, v))
    else if (r.internal.fd)
      r.copy(index = r.setIndexReg(RegNames.IY, v))
    else
      r.copy(regFile1 = r.setBaseReg16(regName, v))
  }

  // inc r
  private def inc8(registers: Registers): Registers = {
    val regName = reg8Bit(registers.internal.y)
    val v = registers.getReg(regName).inc8
    //
    var flags = if (registers.isC) 0x01 else 0x00 // n,c
    if ((v & 0x0F) == 0x00) flags = flags | FLAG_H
    if (v == 0x80) flags = flags | FLAG_PV
    if (v == 0) flags = flags | FLAG_Z
    flags = flags | (v & 0xa8) // s,f5,f3
    //
    if (regName == RegNames.DATA8) {
      if (registers.dd || registers.fd)
        registers.copy(regFile1 = registers.regFile1.copy(f = flags, data8 = Option(v), data16 = None, wz = Option(registers.getReg16(RegNames.DATA16))))
      else
        registers.copy(regFile1 = registers.regFile1.copy(f = flags, data8 = Option(v), data16 = None, wz = Option(registers.getReg16(RegNames.H))))
    }
    else
      registers.copy(regFile1 = registers.setResult8(regName, v, flags))
  }

  // dec r
  private def dec8(r: Registers): Registers = {
    val regName = reg8Bit(r.internal.y)
    val v = r.getReg(regName).dec8
    //
    var flags = if (r.isC) 0x03 else 0x02 // n,c
    if ((v & 0x0f) == 0x0f) flags = flags | FLAG_H
    if (v == 0x7f) flags = flags | FLAG_PV
    if (v == 0) flags = flags | FLAG_Z
    flags = flags | (v & 0xa8) // s,f5,f3
    //
    if (regName == RegNames.DATA8) {
      if (r.dd || r.fd)
        r.copy(regFile1 = r.regFile1.copy(f = flags, data8 = Option(v), data16 = None, wz = Option(r.getReg16(RegNames.DATA16))))
      else
        r.copy(regFile1 = r.regFile1.copy(f = flags, data8 = Option(v), data16 = None, wz = Option(r.getReg16(RegNames.H))))
    }
    else
      r.copy(regFile1 = r.setResult8(regName, v, flags))
  }

  // LD / ADD 16
  private def ldadd16(r: Registers): Registers = {
    var rf1 = r.regFile1
    var cr = r.control
    var ixiy = r.index
    if (r.internal.q == 0) {
      // LD rr,(nn)
      val v = rf1.data16.get
      val lsb = v.lsb
      val msb = v.msb
      r.internal.p match {
        case 0 => rf1 = rf1.copy(b = msb, c = lsb)
        case 1 => rf1 = rf1.copy(d = msb, e = lsb)
        case 2 if r.internal.dd => ixiy = r.setResultIX(v)
        case 2 if r.internal.fd => ixiy = r.setResultIY(v)
        case 2 => rf1 = rf1.copy(h = msb, l = lsb)
        case 3 => cr = cr.copy(sp = v)
      }
      r.copy(regFile1 = rf1.copy(data16 = None), index = ixiy, control = cr)
    } else {
      // ADD HL/IX/IY,rr
      val hl = r.getReg16Index(RegNames.H)
      val rr = r.getReg16Index(reg16Bit(r.internal.p))
      val v = hl + rr
      // sort out flags
      val h = (((hl & 0x0FFF) + (rr & 0x0FFF)) & 0xF000) != 0 // half carry in msb
      val f5 = (v & 0x2000) != 0
      val f3 = (v & 0x0800) != 0
      val c = v > 0xFFFF
      //
      val regFile = r.setFlags(f5f = Option(f5), hf = Option(h), f3f = Option(f3), nf = RESET_FLAG, cf = Option(c)).copy(data16 = None)
      if (r.dd)
        r.copy(index = r.setResultIX(v), regFile1 = regFile)
      else if (r.fd)
        r.copy(index = r.setResultIY(v), regFile1 = regFile)
      else
        r.copy(regFile1 = regFile.copy(h = v.msb, l = v.lsb))
    }
  }

  // Instructions 0x40 -> 0x7F
  // Instructions 0x40 -> 0x7F
  // Instructions 0x40 -> 0x7F

  // standard 8 bit ld src,dst instrucitons
  private def load8(r: Registers): Registers = {
    val v = r.getReg(reg8Bit(r.internal.z))
    // 0 1 2 3 4 5 6 7
    // B C D E H L M A
    r.copy(regFile1 =
      r.internal.y match {
        case 0 => r.regFile1.copy(b = v)
        case 1 => r.regFile1.copy(c = v)
        case 2 => r.regFile1.copy(d = v)
        case 3 => r.regFile1.copy(e = v)
        case 4 => r.regFile1.copy(h = v)
        case 5 => r.regFile1.copy(l = v)
        case 6 => r.regFile1.copy(data8 = Option(v), data16 = None, wz = Option(r.getReg16(RegNames.H)))
        case 7 => r.regFile1.copy(a = v)
      }
    )
  }

  // standard 8 bit add src,dst instrucitons
  private def add8(r: Registers): Registers = {
    var local_reg_A = r.getA
    val right = if (r.dd || r.fd) r.getReg(reg8BitIXIY(r.internal.z)) else r.getReg(reg8Bit(r.internal.z))
    var flags = r.getReg(RegNames.F)
    //
    flags = if (getHalfCarryFlagAdd(local_reg_A, right)) flags | FLAG_H else flags & 0xef
    flags = if (getOverflowFlagAdd(local_reg_A, right)) flags | FLAG_PV else flags & 0xfb
    local_reg_A = local_reg_A + right
    flags = if ((local_reg_A & 0x0080) != 0) flags | FLAG_S else flags & 0x7f // s
    flags = if ((local_reg_A & 0xff00) != 0) flags | FLAG_C else flags & 0xfe // c
    local_reg_A = local_reg_A & 0x00ff
    flags = if (local_reg_A == 0) flags | 0x40 else flags & 0xbf
    flags = flags & 0xfd // n
    flags = if (local_reg_A.f3.get) flags | 0x08 else flags & 0xf7
    flags = if (local_reg_A.f5.get) flags | 0x20 else flags & 0xdf
    //
    r.copy(regFile1 = r.regFile1.copy(a = local_reg_A, f = flags))
  }


  // standard 8 bit adc src,dst instrucitons
  private def adc8(r: Registers): Registers = {
    var local_reg_A = r.getA
    val right = if (r.dd || r.fd) r.getReg(reg8BitIXIY(r.internal.z)) else r.getReg(reg8Bit(r.internal.z))
    var flags = r.getReg(RegNames.F)
    //
    flags = if (getHalfCarryFlagAdd(local_reg_A, right, r.isC)) flags | 0x10 else flags & 0xef
    flags = if (getOverflowFlagAdd(local_reg_A, right, r.isC)) flags | 0x04 else flags & 0xfb
    local_reg_A = local_reg_A + right + (if (r.isC) 1 else 0)
    flags = if ((local_reg_A & 0x0080) != 0) flags | 0x80 else flags & 0x7f // s
    flags = if ((local_reg_A & 0xff00) != 0) flags | 0x01 else flags & 0xfe // c
    local_reg_A = local_reg_A & 0x00ff
    flags = if (local_reg_A == 0) flags | 0x40 else flags & 0xbf
    flags = flags & 0xfd // n
    flags = if (local_reg_A.f3.get) flags | 0x08 else flags & 0xf7
    flags = if (local_reg_A.f5.get) flags | 0x20 else flags & 0xdf
    //
    r.copy(regFile1 = r.regFile1.copy(a = local_reg_A, f = flags))
  }


  // standard 8 bit sub src,dst instrucitons
  private def sub8(r: Registers): Registers = {
    var local_reg_A = r.getA
    val right = if (r.dd || r.fd) r.getReg(reg8BitIXIY(r.internal.z)) else r.getReg(reg8Bit(r.internal.z))
    var flags = r.getReg(RegNames.F)
    //
    flags = if (getHalfCarryFlagSub(local_reg_A, right)) flags | 0x10 else flags & 0xef // h
    flags = if (getOverflowFlagSub(local_reg_A, right)) flags | 0x04 else flags & 0xfb // pv
    local_reg_A = local_reg_A - right
    //
    flags = if ((local_reg_A & 0x0080) != 0) flags | 0x80 else flags & 0x7f // s
    flags = if ((local_reg_A & 0xff00) != 0) flags | 0x01 else flags & 0xfe // c
    local_reg_A = local_reg_A & 0x00ff
    flags = if (local_reg_A == 0) flags | 0x40 else flags & 0xbf // z
    flags = flags | 0x02 // n
    flags = if (local_reg_A.f3.get) flags | 0x08 else flags & 0xf7 // f3
    flags = if (local_reg_A.f5.get) flags | 0x20 else flags & 0xdf // f5
    //
    r.copy(regFile1 = r.regFile1.copy(a = local_reg_A, f = flags))
  }


  // standard 8 bit add/adc src,dst instrucitons
  private def sbc8(r: Registers): Registers = {
    var local_reg_A = r.getA
    val right = if (r.dd || r.fd) r.getReg(reg8BitIXIY(r.internal.z)) else r.getReg(reg8Bit(r.internal.z))
    var flags = r.getReg(RegNames.F)
    //
    flags = if (getHalfCarryFlagSub(local_reg_A, right, r.isC)) flags | 0x10 else flags & 0xef // h
    flags = if (getOverflowFlagSub(local_reg_A, right, r.isC)) flags | 0x04 else flags & 0xfb // pv
    local_reg_A = local_reg_A - right - (if (r.isC) 1 else 0)
    //
    flags = if ((local_reg_A & 0x0080) != 0) flags | 0x80 else flags & 0x7f
    flags = if ((local_reg_A & 0xff00) != 0) flags | 0x01 else flags & 0xfe
    local_reg_A = local_reg_A & 0x00ff
    flags = if (local_reg_A == 0) flags | 0x40 else flags & 0xbf
    flags = flags | 0x02 // n
    flags = if (local_reg_A.f3.get) flags | 0x08 else flags & 0xf7
    flags = if (local_reg_A.f5.get) flags | 0x20 else flags & 0xdf
    //
    r.copy(regFile1 = r.regFile1.copy(a = local_reg_A, f = flags))
  }


  // standard 8 bit cp src,dst instrucitons
  private def cp8(r: Registers): Registers = {
    val a = r.getA
    val right = if (r.dd || r.fd) r.getReg(reg8BitIXIY(r.internal.z)) else r.getReg(reg8Bit(r.internal.z))
    val wans = a - right
    val ans = wans & 0xff
    //
    var flags = 0x02 // n
    flags = if ((ans & 0x0080) != 0) flags | 0x80 else flags & 0x7f // s
    flags = if (right.f3.get) flags | 0x08 else flags & 0xf7 // f3
    flags = if (right.f5.get) flags | 0x20 else flags & 0xdf // f5
    flags = if (ans == 0) flags | 0x40 else flags & 0xbf // z
    flags = if ((wans & 0x100) != 0) flags | 0x01 else flags & 0xfe // c

    flags = if ((((ans & 0x0f) - (right & 0x0f)) & 0x0010) != 0) flags | 0x10 else flags & 0xef // c

    flags = if (((a ^ right) & (a ^ ans) & 0x80) != 0) flags | 0x04 else flags & 0xfb // pv

    r.copy(regFile1 = r.regFile1.copy(f = flags))
  }


  // standard 8 bit and src,dst instructions
  private def and8(r: Registers): Registers = {
    val right = if (r.dd || r.fd) r.getReg(reg8BitIXIY(r.internal.z)) else r.getReg(reg8Bit(r.internal.z))
    val reg_A = r.getA & right
    var flags = 0x10 // h / n  /c
    //
    flags = if ((reg_A & 0x0080) != 0) flags | 0x80 else flags & 0x7f // s
    flags = if (reg_A == 0) flags | 0x40 else flags & 0xbf // z
    flags = if (getParityFlag(reg_A)) flags | 0x04 else flags & 0xfb // pv
    flags = if (reg_A.f3.get) flags | 0x08 else flags & 0xf7 // f3
    flags = if (reg_A.f5.get) flags | 0x20 else flags & 0xdf // f5
    //
    r.copy(regFile1 = r.regFile1.copy(a = reg_A, f = flags))
  }


  // standard 8 bit and src,dst instructions
  private def or8(r: Registers): Registers = {
    val right = if (r.dd || r.fd) r.getReg(reg8BitIXIY(r.internal.z)) else r.getReg(reg8Bit(r.internal.z))
    val reg_A = r.getA | right
    var flags = 0 //  h /n / c
    //
    flags = if ((reg_A & 0x0080) != 0) flags | 0x80 else flags & 0x7f // s
    flags = if (reg_A == 0) flags | 0x40 else flags & 0xbf // z
    flags = if (getParityFlag(reg_A)) flags | 0x04 else flags & 0xfb // pv
    flags = if (reg_A.f3.get) flags | 0x08 else flags & 0xf7 // f3
    flags = if (reg_A.f5.get) flags | 0x20 else flags & 0xdf // f5
    //
    r.copy(regFile1 = r.regFile1.copy(a = reg_A, f = flags))
  }

  // standard 8 bit and src,dst instructions
  private def xor8(r: Registers): Registers = {
    val right = if (r.dd || r.fd) r.getReg(reg8BitIXIY(r.internal.z)) else r.getReg(reg8Bit(r.internal.z))
    val reg_A = r.getA ^ right
    var flags = 0 // h,n,c
    //
    flags = if ((reg_A & 0x0080) != 0) flags | 0x80 else flags & 0x7f // s
    flags = if (reg_A == 0) flags | 0x40 else flags & 0xbf // z
    flags = if (getParityFlag(reg_A)) flags | 0x04 else flags & 0xfb // pv
    flags = if (reg_A.f3.get) flags | 0x08 else flags & 0xf7 // f3
    flags = if (reg_A.f5.get) flags | 0x20 else flags & 0xdf // f5
    //
    r.copy(regFile1 = r.regFile1.copy(a = reg_A, f = flags))
  }

  // ret cc
  private def retcc(r: Registers): Registers = {
    if (cc(r.internal.y, r))
      ret(r)
    else
      r
  }

  // ret
  private def ret(r: Registers): Registers = {
    val sp = r.getSP
    val addr = memory.pop(sp)
    r.copy(control = r.control.copy(pc = addr, sp = sp.inc16.inc16))
  }

  // jp cc
  private def jpcc(r: Registers): Registers = {
    if (cc(r.internal.y, r))
      jp(r)
    else
      r
  }

  // call cc
  private def callcc(r: Registers): Registers = {
    if (cc(r.internal.y, r))
      call(r)
    else
      r
  }

  // rst
  private def rst(registers: Registers): Registers = {
    val addr = registers.internal.y * 8
    memory.push(registers.getSP, registers.getPC)
    registers.copy(control = registers.control.copy(pc = addr, sp = (registers.getSP - 2).limit16))
  }

  // relative jump
  private def jr(r: Registers): Registers = {
    var offset = r.getReg(RegNames.DATA8)
    if (offset > 0x007F) offset = offset - 0x0100
    val pc = (r.getPC + offset).limit16
    r.copy(control = r.setPC(pc))
  }

  // absolute jump
  private def jp(r: Registers): Registers = {
    r.copy(control = r.setPC(r.getReg(RegNames.DATA16)))
  }

  // absolute call
  private def call(r: Registers): Registers = {
    memory.push(r.getSP, r.getPC)
    r.copy(control = r.control.copy(pc = r.getReg(RegNames.DATA16), sp = (r.getSP - 2).limit16))
  }

  // Execute CB prefix
  // Execute CB prefix
  // Execute CB prefix

  private def executeCB(regs: Registers): Registers = {
    val r = regs.copy(internal = regs.internal.copy(cb = false))
    val reg = reg8Bit(r.internal.z)
    var v = r.getReg(reg)
    r.internal.x match {
      case 0 => // rot[y] r[z]
        var flags = 0x00
        r.internal.y match {
          case 0 => // rlc
            v = v << 1
            if ((v & 0xFF00) != 0) {
              v = v | 0x01
              flags = 0x01
            }
            v = v.limit8
          case 1 => // rrc
            val c = (v & 0x01) != 0
            v = v >>> 1
            if (c) {
              v = v | 0x80
              flags = 0x01
            }
          case 2 => // rl
            v = v << 1
            if (r.isC) v = v | 0x01
            if ((v & 0xFF00) != 0) flags = flags | FLAG_C
            v = v.limit8
          case 3 => // rr
            if ((v & 0x01) != 0) flags = flags | FLAG_C
            v = v >>> 1
            if (r.isC) v = v | 0x80
          case 4 => // sla
            v = v << 1
            if ((v & 0xFF00) != 0) flags = flags | FLAG_C
            v = v.limit8
          case 5 => // sra
            if ((v & 0x01) != 0) flags = flags | FLAG_C
            val s = (v & 0x80) != 0
            v = v >>> 1
            if (s) v = v | 0x80
          case 6 => // sll
            v = (v << 1) | 0x01
            if ((v & 0xFF00) != 0) flags = flags | FLAG_C
            v = v.limit8
          case 7 => // srl
            if ((v & 0x01) != 0) flags = flags | FLAG_C
            v = v >>> 1
        }
        if (reg == RegNames.DATA8)
          r.copy(regFile1 = r.setResult8(reg, v, setFlags_S_Z_F5_F3_P(v, flags)).copy(data16 = None, wz = Option(regs.getReg16(RegNames.H))))
        else
          r.copy(regFile1 = r.setResult8(reg, v, setFlags_S_Z_F5_F3_P(v, flags)))
      //
      case 1 => // BIT y, r[z]
        val z = 0 == (v & testBit(r.internal.y))
        val s = (7 == r.internal.y) && (0 != (v & 0x80))
        if (reg == RegNames.DATA8) {
          val rf1 = r.setFlags(sf = Option(s), zf = Option(z), hf = SET_FLAG, pvf = Option(z), nf = RESET_FLAG)
          r.copy(regFile1 = rf1.copy(data8 = None, data16 = None))
        }
        else
          r.copy(regFile1 = r.setFlags(sf = Option(s), zf = Option(z), hf = SET_FLAG, f5f = v.f5, pvf = Option(z), f3f = v.f3, nf = RESET_FLAG))

      case 2 => // RES y, r[z]
        v = v & resetBit(r.internal.y)
        if (reg == RegNames.DATA8)
          r.copy(regFile1 = r.regFile1.copy(data8 = Option(v), data16 = None, wz = Option(r.getReg16(RegNames.H))))
        else
          r.copy(regFile1 = r.setResult8(reg, v))
      case 3 => // SET y, r[z]
        v = v | testBit(r.internal.y)
        if (reg == RegNames.DATA8)
          r.copy(regFile1 = r.regFile1.copy(data8 = Option(v), data16 = None, wz = Option(r.getReg16(RegNames.H))))
        else
          r.copy(regFile1 = r.setResult8(reg, v))
    }
  }

  // Execute DD FD prefix
  // Execute DD FD prefix
  // Execute DD FD prefix

  private def executeDDFD(r: Registers): Registers = {

    r.internal.x match {
      case 0 =>
        r.internal.inst match {
          case 0x09 => ldadd16(r)
          case 0x19 => ldadd16(r)
          case 0x21 => ldadd16(r)
          case 0x22 => indirectLoad(r)
          case 0x23 => incDec16(r)
          case 0x24 => inc8IXIY(r)
          case 0x25 => dec8IXIY(r)
          case 0x26 => ldImmediate8IXIY(r)
          case 0x29 => ldadd16(r)
          case 0x2A => indirectLoad(r)
          case 0x2B => incDec16(r)
          case 0x2C => inc8IXIY(r)
          case 0x2D => dec8IXIY(r)
          case 0x2E => ldImmediate8IXIY(r)
          case 0x34 => inc8(r)
          case 0x35 => dec8(r)
          case 0x36 => ldImmediate8IXIY(r)
          case 0x39 => ldadd16(r)
          case _ => throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (ddfd): " + Utils.toHex8(r.internal.inst))
        }
      case 1 =>
        load8IXIY(r)
      case 2 =>
        // add/adc/sub/sbc/and/xor/or/cp
        if (r.internal.z == 4 || r.internal.z == 5 || r.internal.z == 6) {
          r.internal.y match {
            case 0 => add8(r)
            case 1 => adc8(r)
            case 2 => sub8(r)
            case 3 => sbc8(r)
            case 4 => and8(r)
            case 5 => xor8(r)
            case 6 => or8(r)
            case 7 => cp8(r)
          }
        }
        else
          throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (ddfd): " + Utils.toHex8(r.internal.inst))

      case 3 =>
        r.internal.inst match {
          case 0xCB =>
            r.copy(internal = r.internal.copy(single = false, cb = true))
          case 0xE1 => // pop
            val v = memory.pop(r.getSP)
            val cr = r.setSP((r.getSP + 2).limit16)
            if (r.internal.dd)
              r.copy(control = cr, index = r.index.copy(ix = v))
            else
              r.copy(control = cr, index = r.index.copy(iy = v))
          //
          case 0xE3 => // ex (sp),ix
            val v = memory.pop(r.getSP)
            memory.push((r.getSP + 2).limit16, r.getReg16(RegNames.H))
            if (r.internal.dd)
              r.copy(index = r.index.copy(ix = v))
            else
              r.copy(index = r.index.copy(iy = v))
          //
          case 0xE5 => // push
            if (r.internal.dd)
              memory.push(r.getSP, r.getReg16(RegNames.IX))
            else
              memory.push(r.getSP, r.getReg16(RegNames.IY))
            val cr = r.setSP((r.getSP - 2).limit16)
            r.copy(control = cr)
          //
          case 0xE9 => // jp (ixiy)
            if (r.internal.dd)
              r.copy(control = r.setPC(r.getReg16(RegNames.IX)))
            else
              r.copy(control = r.setPC(r.getReg16(RegNames.IY)))
          //
          case 0xF9 => // ld sp,ix
            if (r.internal.dd)
              r.copy(control = r.setSP(r.getReg16(RegNames.IX)))
            else
              r.copy(control = r.setSP(r.getReg16(RegNames.IY)))
          //
          case _ => throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (ddfd): " + Utils.toHex8(r.internal.inst))
        }
    }
  }

  // standard 8 bit ld src,dst instrucitons
  private def load8IXIY(registers: Registers): Registers = {
    val srcReg = registers.internal.z
    val v = registers.getReg(reg8BitIXIY(srcReg))
    // 0 1 2 3 4 5 6 7
    // B C D E H L M A
    var r = registers.regFile1
    if (6 == srcReg) { // ld rr, (ixiy+dd)
      registers.internal.y match {
        case 0 => r = r.copy(b = v)
        case 1 => r = r.copy(c = v)
        case 2 => r = r.copy(d = v)
        case 3 => r = r.copy(e = v)
        case 4 => r = r.copy(h = v)
        case 5 => r = r.copy(l = v)
        case 7 => r = r.copy(a = v)
        case _ => throw new UndefOpcode("Addr: " + Utils.toHex16(registers.getPC) + " inst (load8IXIY): " + Utils.toHex8(registers.internal.inst))
      }
      registers.copy(regFile1 = r)
    }
    else {
      registers.internal.y match {
        case 0 => registers.copy(regFile1 = r.copy(b = v))
        case 1 => registers.copy(regFile1 = r.copy(c = v))
        case 2 => registers.copy(regFile1 = r.copy(d = v))
        case 3 => registers.copy(regFile1 = r.copy(e = v))
        case 4 => // ixh
          if (registers.dd) {
            val ix_val = (registers.getReg16(RegNames.IX) & 0x00FF) | (v << 8)
            registers.copy(index = registers.index.copy(ix = ix_val))
          }
          else {
            val iy_val = (registers.getReg16(RegNames.IY) & 0x00FF) | (v << 8)
            registers.copy(index = registers.index.copy(iy = iy_val))
          }
        case 5 => // ixl
          if (registers.dd) {
            val ix_val = (registers.getReg16(RegNames.IX) & 0xFF00) | v
            registers.copy(index = registers.index.copy(ix = ix_val))
          }
          else {
            val iy_val = (registers.getReg16(RegNames.IY) & 0xFF00) | v
            registers.copy(index = registers.index.copy(iy = iy_val))
          }
        case 6 => registers.copy(regFile1 = r.copy(data8 = Option(registers.getReg(reg8Bit(srcReg))), data16 = None, wz = Option(registers.getReg16(RegNames.DATA16))))
        case 7 => registers.copy(regFile1 = r.copy(a = v))
      }
    }
  }

  // ld r,nn
  private def ldImmediate8IXIY(r: Registers): Registers = {
    val regName = reg8BitIXIY(r.internal.y)
    val v = r.getReg(RegNames.DATA8)

    regName match {
      case RegNames.DATA8 => r.copy(regFile1 = r.regFile1.copy(data16 = None, wz = Option(r.getReg16(RegNames.DATA16))))
      case RegNames.IXIYH if r.dd =>
        val ix_val = (r.getReg16(RegNames.IX) & 0x00FF) | (v << 8)
        r.copy(index = r.index.copy(ix = ix_val))
      case RegNames.IXIYL if r.dd =>
        val ix_val = (r.getReg16(RegNames.IX) & 0xFF00) | v
        r.copy(index = r.index.copy(ix = ix_val))
      case RegNames.IXIYH if r.fd =>
        val iy_val = (r.getReg16(RegNames.IY) & 0x00FF) | (v << 8)
        r.copy(index = r.index.copy(iy = iy_val))
      case RegNames.IXIYL if r.fd =>
        val iy_val = (r.getReg16(RegNames.IY) & 0xFF00) | v
        r.copy(index = r.index.copy(iy = iy_val))
      case _ => throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (ldImmediate8IXIY): " + Utils.toHex8(r.internal.inst))
    }
  }

  // inc r
  private def inc8IXIY(registers: Registers): Registers = {
    val regName = reg8BitIXIY(registers.internal.y)
    val v = registers.getReg(regName).inc8
    //
    val h = (v & 0x0F) == 0x00
    val pv = v == 0x80
    val s = (v & 0x80) != 0
    val z = v == 0
    //
    val wz_val = if (regName == RegNames.DATA8) Option(registers.getReg16(RegNames.DATA16)) else None
    //
    val rf1 = registers.setFlags(sf = Option(s), zf = Option(z), f5f = v.f5, hf = Option(h), f3f = v.f3, pvf = Option(pv), nf = RESET_FLAG).copy(wz = wz_val)
    if (registers.dd) {
      var ix_val = registers.getReg16(RegNames.IX)
      if (regName == RegNames.IXIYH)
        ix_val = (ix_val & 0x00FF) | (v << 8)
      else
        ix_val = (ix_val & 0xFF00) | v
      registers.copy(regFile1 = rf1, index = registers.index.copy(ix = ix_val))
    }
    else {
      var iy_val = registers.getReg16(RegNames.IY)
      if (regName == RegNames.IXIYH)
        iy_val = (iy_val & 0x00FF) | (v << 8)
      else
        iy_val = (iy_val & 0xFF00) | v
      registers.copy(regFile1 = rf1, index = registers.index.copy(iy = iy_val))
    }
  }

  // dec r
  private def dec8IXIY(registers: Registers): Registers = {
    val regName = reg8BitIXIY(registers.internal.y)
    val v = registers.getReg(regName).dec8
    //
    val h = (v & 0x0f) == 0x0f
    val pv = v == 0x7f
    val s = (v & 0x80) != 0
    val z = v == 0
    //
    val wz_val = if (regName == RegNames.DATA8) Option(registers.getReg16(RegNames.DATA16)) else None
    //
    val rf1 = registers.setFlags(sf = Option(s), zf = Option(z), f5f = v.f5, hf = Option(h), f3f = v.f3, pvf = Option(pv), nf = SET_FLAG).copy(wz = wz_val, data16 = None)
    if (registers.dd) {
      var ix_val = registers.getReg16(RegNames.IX)
      if (regName == RegNames.IXIYH)
        ix_val = (ix_val & 0x00FF) | (v << 8)
      else
        ix_val = (ix_val & 0xFF00) | v
      registers.copy(regFile1 = rf1, index = registers.index.copy(ix = ix_val))
    }
    else {
      var iy_val = registers.getReg16(RegNames.IY)
      if (regName == RegNames.IXIYH)
        iy_val = (iy_val & 0x00FF) | (v << 8)
      else
        iy_val = (iy_val & 0xFF00) | v
      registers.copy(regFile1 = rf1, index = registers.index.copy(iy = iy_val))
    }
  }

  // Execute ED prefix
  // Execute ED prefix
  // Execute ED prefix

  private def ldi(r: Registers): BaseRegisters = {
    val hl = r.getReg16(RegNames.H).inc16
    val target = r.getReg16(RegNames.D)
    val de = target.inc16
    val bc = r.getReg16(RegNames.B).dec16
    var flags = r.getReg(RegNames.F) & 0xC1 // f5=0,h=0,f3=0,pv=0,n=0
    val f5f3 = r.getA + r.getReg(RegNames.DATA8)
    if ((f5f3 & 0x02) != 0) flags = flags | 0x20 // weird
    if ((f5f3 & 0x08) != 0) flags = flags | 0x08
    if (bc != 0) flags = flags | 0x04
    r.regFile1.copy(f = flags, h = hl.msb, l = hl.lsb, d = de.msb, e = de.lsb, b = bc.msb, c = bc.lsb, wz = Option(target))
  }

  private def ldd(r: Registers): BaseRegisters = {
    val hl = r.getReg16(RegNames.H).dec16
    val target = r.getReg16(RegNames.D)
    val de = target.dec16
    val bc = r.getReg16(RegNames.B).dec16
    var flags = r.getReg(RegNames.F) & 0xC1 // f5=0,h=0,f3=0,pv=0,n=0
    val f5f3 = r.getA + r.getReg(RegNames.DATA8)
    if ((f5f3 & 0x02) != 0) flags = flags | 0x20 // weird
    if ((f5f3 & 0x08) != 0) flags = flags | 0x08
    if (bc != 0) flags = flags | 0x04
    r.regFile1.copy(f = flags, h = hl.msb, l = hl.lsb, d = de.msb, e = de.lsb, b = bc.msb, c = bc.lsb, wz = Option(target))
  }

  private def cpi(r: Registers): BaseRegisters = {
    val hl = r.getReg16(RegNames.H).inc16
    val bc = r.getReg16(RegNames.B).dec16
    var flags = (r.getReg(RegNames.F) & 0x01) | 0x02 // c = unchanged, n=1
    var v = (r.getA - r.getReg(RegNames.DATA8)).limit8
    flags = flags | (v & 0x80) // s
    if (v == 0) flags = flags | 0x40 // z
    val h = getHalfCarryFlagSub(r.getA, r.getReg(RegNames.DATA8), carry = false)
    //
    if (h) {
      v = v - 1
      flags = flags | 0x10 // h
    }
    if ((v & 0x08) != 0) flags = flags | 0x08 // weird f3
    if ((v & 0x02) != 0) flags = flags | 0x20 // wierd f5
    if (bc != 0) flags = flags | 0x04 // pv
    r.regFile1.copy(f = flags, h = hl.msb, l = hl.lsb, b = bc.msb, c = bc.lsb)
  }

  private def cpd(r: Registers): BaseRegisters = {
    val hl = r.getReg16(RegNames.H).dec16
    val bc = r.getReg16(RegNames.B).dec16
    var flags = (r.getReg(RegNames.F) & 0x01) | 0x02 // c = unchanged, n=1
    var v = (r.getA - r.getReg(RegNames.DATA8)).limit8
    //
    flags = flags | (v & 0x80) // s
    if (v == 0) flags = flags | 0x40 // z
    val h = getHalfCarryFlagSub(r.getA, r.getReg(RegNames.DATA8), carry = false)
    //
    if (h) {
      v = v - 1
      flags = flags | 0x10 // h
    }
    if ((v & 0x08) != 0) flags = flags | 0x08 // weird f3
    if ((v & 0x02) != 0) flags = flags | 0x20 // wierd f5
    if (bc != 0) flags = flags | 0x04 // pv
    r.regFile1.copy(f = flags, h = hl.msb, l = hl.lsb, b = bc.msb, c = bc.lsb)
  }

  private def executeED(r: Registers): Registers = {
    if (1 == r.internal.x)
      executeEDBlock1(r)
    else
      r.internal.inst match {
        case 0xA0 => r.copy(regFile1 = ldi(r)) // LDI
        case 0xA1 => r.copy(regFile1 = cpi(r)) // CPI
        case 0xA8 => r.copy(regFile1 = ldd(r)) // LDD
        case 0xA9 => r.copy(regFile1 = cpd(r)) // CPD
        case 0xB0 =>
          // LDIR
          val rf1 = ldi(r)
          if ((0 != rf1.b) || (0 != rf1.c))
            r.copy(regFile1 = rf1, control = r.control.copy(pc = r.control.pc.dec16.dec16))
          else
            r.copy(regFile1 = rf1)
        case 0xB1 =>
          // CPIR
          val rf1 = cpi(r)
          val z = (rf1.f & 0x40) != 0
          if (!z && ((0 != rf1.b) || (0 != rf1.c)))
            r.copy(regFile1 = rf1, control = r.control.copy(pc = r.control.pc.dec16.dec16))
          else
            r.copy(regFile1 = rf1)
        case 0xB8 =>
          // LDDR
          val rf1 = ldd(r)
          if ((0 != rf1.b) || (0 != rf1.c))
            r.copy(regFile1 = rf1, control = r.control.copy(pc = r.control.pc.dec16.dec16))
          else
            r.copy(regFile1 = rf1)
        case 0xB9 =>
          // CPDR
          val rf1 = cpd(r)
          val z = (rf1.f & 0x40) != 0
          if (!z && ((0 != rf1.b) || (0 != rf1.c)))
            r.copy(regFile1 = rf1, control = r.control.copy(pc = r.control.pc.dec16.dec16))
          else
            r.copy(regFile1 = rf1)
        case _ => throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (ed): " + Utils.toHex8(r.internal.inst))
      }
  }

  private def executeEDBlock1(r: Registers): Registers = {
    r.internal.z match {
      case 2 =>
        val rh = r.getReg16(reg16Bit(r.p))
        val hl = r.getReg16(RegNames.H)
        val cv = if (r.isC) 1 else 0
        if (r.q == 0) {
          // sbc hl,rr
          val raw = hl - rh - cv
          val res = raw & 0xFFFF
          //
          val s = (res & 0x8000) != 0
          val f3 = (res & 0x0800) != 0
          val f5 = (res & 0x2000) != 0
          val z = res == 0
          val c = raw < 0
          val h = (((hl & 0x0fff) - (rh & 0x0fff) - cv) & 0x1000) != 0
          val pv = getOverflowFlagSub16(hl, rh, r.isC)
          //
          val rf1 = r.setFlags(sf = Option(s), f3f = Option(f3), f5f = Option(f5), cf = Option(c), zf = Option(z), nf = Option(true), hf = Option(h), pvf = Option(pv))
          r.copy(regFile1 = rf1.copy(h = res.msb, l = res.lsb))
        }
        else {
          // adc hl,rr
          val raw = hl + rh + cv
          val res = raw & 0xFFFF
          //
          val s = (res & 0x8000) != 0
          val f3 = (res & 0x0800) != 0
          val f5 = (res & 0x2000) != 0
          val z = res == 0
          val c = raw > 0xFFFF
          val h = (((hl & 0x0fff) + (rh & 0x0fff) + cv) & 0x1000) != 0
          val pv = getOverflowFlagAdd16(hl, rh, r.isC)
          //
          val rf1 = r.setFlags(sf = Option(s), f3f = Option(f3), f5f = Option(f5), cf = Option(c), zf = Option(z), nf = Option(false), hf = Option(h), pvf = Option(pv))
          r.copy(regFile1 = rf1.copy(h = res.msb, l = res.lsb))
        }
      case 3 =>
        if (r.internal.q == 0) {
          memory.setMemory16(r.getReg16(RegNames.DATA16), r.getReg16(reg16Bit(r.internal.p)))
          r.copy(regFile1 = r.regFile1.copy(data16 = None))
        }
        else {
          val v = r.getReg16(RegNames.DATA16)
          if (r.internal.p == 3) // sp
            r.copy(regFile1 = r.regFile1.copy(data16 = None), control = r.setSP(v))
          else
            r.copy(regFile1 = r.setBaseReg16(reg16Bit(r.internal.p), v).copy(data16 = None))
        }
      case 4 => // neg
        val raw = 0 - r.getA
        val c = (raw & 0xFF00) != 0
        val v = raw.limit8
        val s = (raw & FLAG_S) != 0
        val z = v == 0
        val h = getHalfCarryFlagSub(0, r.getA, carry = false)
        val pv = getOverflowFlagSub(0, r.getA, carry = false)
        val n = true
        //
        r.copy(regFile1 = r.setResultA(v, sf = Option(s), zf = Option(z), f5f = v.f5, hf = Option(h), f3f = v.f3, pvf = Option(pv), nf = Option(n), cf = Option(c)))
      case 7 =>
        r.internal.y match {
          case 4 => rrd(r)
          case 5 => rld(r)
          case 6 => r // nop
          case 7 => r // nop
        }
      case _ => throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (ed): " + Utils.toHex8(r.internal.inst))
    }
  }

  def rrd(r: Registers): Registers = {
    val temp = r.getReg(RegNames.DATA8)
    val nibble1 = (r.getA & 0x00F0) >> 4
    val nibble2 = r.getA & 0x000F
    val nibble3 = (temp & 0x00F0) >> 4
    val nibble4 = temp & 0x000F
    val reg_a = (nibble1 << 4) | nibble4
    val mem = (nibble2 << 4) | nibble3
    //
    val regs = r.copy(regFile1 = r.regFile1.copy(a = reg_a, data8 = Option(mem), wz = Option(r.getReg16(RegNames.H))))
    //
    val s = (reg_a & FLAG_S) != 0
    val z = reg_a == 0
    val pv = getParityFlag(reg_a)
    regs.copy(regFile1 = regs.setFlags(sf = Option(s), zf = Option(z), f5f = reg_a.f5, hf = RESET_FLAG, f3f = reg_a.f3, pvf = Option(pv), nf = RESET_FLAG))
  }

  def rld(r: Registers): Registers = {
    val temp = r.getReg(RegNames.DATA8)
    val nibble1 = (r.getA & 0x00F0) >> 4
    val nibble2 = r.getA & 0x000F
    val nibble3 = (temp & 0x00F0) >> 4
    val nibble4 = temp & 0x000F
    //
    val reg_a = (nibble1 << 4) | nibble3
    val mem = (nibble4 << 4) | nibble2
    //
    val regs = r.copy(regFile1 = r.regFile1.copy(a = reg_a, data8 = Option(mem), wz = Option(r.getReg16(RegNames.H))))
    //
    val s = (reg_a & FLAG_S) != 0
    val z = reg_a == 0
    val pv = getParityFlag(reg_a)
    regs.copy(regFile1 = regs.setFlags(sf = Option(s), zf = Option(z), f5f = reg_a.f5, hf = RESET_FLAG, f3f = reg_a.f3, pvf = Option(pv), nf = RESET_FLAG))
  }

  // Execute DD FD CB prefix
  // Execute DD FD CB prefix
  // Execute DD FD CB prefix

  private def executeDDFDCB(r: Registers): Registers = {
    r.internal.x match {
      case 0 => rotateDDFDCB(r)
      case 1 => bitDDFDCB(r)
      case 2 => resDDFDCB(r)
      case 3 => setDDFDCB(r)
      case _ => throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (dd cb): " + Utils.toHex8(r.internal.inst))
    }
  }

  private def rotateDDFDCB(r: Registers): Registers = {
    r.internal.z match {
      case 6 => rotate(r)
      case _ => throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (dd cb): " + Utils.toHex8(r.internal.inst))
    }
  }

  private def rotate(r: Registers): Registers = {
    var v = r.getReg(RegNames.DATA8)
    r.internal.y match {
      case 0 => // rlc
        v = v << 1
        val c = (v & 0xFF00) != 0
        if (c) v = v | 0x01
        v = v.limit8
        val s = (v & 0x80) != 0
        val z = v == 0
        val pv = getParityFlag(v)
        val rf1 = r.setFlags(sf = Option(s), zf = Option(z), f5f = v.f5, hf = RESET_FLAG, f3f = v.f3, pvf = Option(pv), nf = RESET_FLAG, cf = Option(c))
        r.copy(regFile1 = rf1.copy(data8 = Option(v), data16 = None, wz = Option(r.getReg16(RegNames.DATA16))))
      case 1 => // rrc
        val c = (v & 0x01) != 0
        v = v >>> 1
        if (c) v = v | 0x80
        val s = (v & 0x80) != 0
        val z = v == 0
        val pv = getParityFlag(v)
        val rf1 = r.setFlags(sf = Option(s), zf = Option(z), f5f = v.f5, hf = RESET_FLAG, f3f = v.f3, pvf = Option(pv), nf = RESET_FLAG, cf = Option(c))
        r.copy(regFile1 = rf1.copy(data8 = Option(v), data16 = None, wz = Option(r.getReg16(RegNames.DATA16))))
      case 2 => // rl
        v = v << 1
        if (r.isC) v = v | 0x01
        val c = (v & 0xFF00) != 0
        v = v.limit8
        val s = (v & 0x80) != 0
        val z = v == 0
        val pv = getParityFlag(v)
        val rf1 = r.setFlags(sf = Option(s), zf = Option(z), f5f = v.f5, hf = RESET_FLAG, f3f = v.f3, pvf = Option(pv), nf = RESET_FLAG, cf = Option(c))
        r.copy(regFile1 = rf1.copy(data8 = Option(v), data16 = None, wz = Option(r.getReg16(RegNames.DATA16))))
      case 3 => // rr
        val c = (v & 0x01) != 0
        v = v >>> 1
        if (r.isC) v = v | 0x80
        val s = (v & 0x80) != 0
        val z = v == 0
        val pv = getParityFlag(v)
        val rf1 = r.setFlags(sf = Option(s), zf = Option(z), f5f = v.f5, hf = RESET_FLAG, f3f = v.f3, pvf = Option(pv), nf = RESET_FLAG, cf = Option(c))
        r.copy(regFile1 = rf1.copy(data8 = Option(v), data16 = None, wz = Option(r.getReg16(RegNames.DATA16))))
      case 4 => // sla
        v = v << 1
        val c = (v & 0xFF00) != 0
        v = v.limit8
        val s = (v & 0x80) != 0
        val z = v == 0
        val pv = getParityFlag(v)
        val rf1 = r.setFlags(sf = Option(s), zf = Option(z), f5f = v.f5, hf = RESET_FLAG, f3f = v.f3, pvf = Option(pv), nf = RESET_FLAG, cf = Option(c))
        r.copy(regFile1 = rf1.copy(data8 = Option(v), data16 = None, wz = Option(r.getReg16(RegNames.DATA16))))
      case 5 => // sra
        val c = (v & 0x01) != 0
        val s = (v & 0x80) != 0
        v = v >>> 1
        if (s) v = v | 0x80
        val z = v == 0
        val pv = getParityFlag(v)
        val rf1 = r.setFlags(sf = Option(s), zf = Option(z), f5f = v.f5, hf = RESET_FLAG, f3f = v.f3, pvf = Option(pv), nf = RESET_FLAG, cf = Option(c))
        r.copy(regFile1 = rf1.copy(data8 = Option(v), data16 = None, wz = Option(r.getReg16(RegNames.DATA16))))
      case 6 => // sll
        v = (v << 1) | 0x01
        val c = (v & 0xFF00) != 0
        v = v.limit8
        val s = (v & 0x80) != 0
        val z = false
        val pv = getParityFlag(v)
        val rf1 = r.setFlags(sf = Option(s), zf = Option(z), f5f = v.f5, hf = RESET_FLAG, f3f = v.f3, pvf = Option(pv), nf = RESET_FLAG, cf = Option(c))
        r.copy(regFile1 = rf1.copy(data8 = Option(v), data16 = None, wz = Option(r.getReg16(RegNames.DATA16))))
      case 7 => // srl
        val c = (v & 0x01) != 0
        v = v >>> 1
        val s = false
        val z = v == 0
        val pv = getParityFlag(v)
        val rf1 = r.setFlags(sf = Option(s), zf = Option(z), f5f = v.f5, hf = RESET_FLAG, f3f = v.f3, pvf = Option(pv), nf = RESET_FLAG, cf = Option(c))
        r.copy(regFile1 = rf1.copy(data8 = Option(v), data16 = None, wz = Option(r.getReg16(RegNames.DATA16))))
    }
  }

  private def bitDDFDCB(r: Registers): Registers = {
    if (r.internal.z == 6) {
      val v = r.getReg(RegNames.DATA8)
      val z = 0 == (v & testBit(r.internal.y))
      val s = (7 == r.internal.y) && (0 != (v & 0x80))
      val rf1 = r.setFlags(sf = Option(s), zf = Option(z), hf = SET_FLAG, pvf = Option(z), nf = RESET_FLAG)
      r.copy(regFile1 = rf1.copy(data8 = None, data16 = None))
    }
    else
      throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (dd cb): " + Utils.toHex8(r.internal.inst))
  }

  private def resDDFDCB(r: Registers): Registers = {
    if (r.internal.z == 6) {
      val v = r.getReg(RegNames.DATA8) & resetBit(r.internal.y)
      r.copy(regFile1 = r.regFile1.copy(data8 = Option(v), data16 = None, wz = Option(r.getReg16(RegNames.DATA16))))
    }
    else
      throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (dd cb): " + Utils.toHex8(r.internal.inst))
  }

  private def setDDFDCB(r: Registers): Registers = {
    if (r.internal.z == 6) {
      val v = r.getReg(RegNames.DATA8) | testBit(r.internal.y)
      r.copy(regFile1 = r.regFile1.copy(data8 = Option(v), data16 = None, wz = Option(r.getReg16(RegNames.DATA16))))
    }
    else
      throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (dd cb): " + Utils.toHex8(r.internal.inst))
  }

  // helpers - helpers - helpers
  // helpers - helpers - helpers
  // helpers - helpers - helpers

  private def setMemory8fromA(r: Registers, addr: Int): Registers = {
    r.copy(regFile1 = r.regFile1.copy(data8 = Option(r.getA), data16 = None, wz = Option(addr)))
  }

  private def setAfromMemory8(r: Registers): Registers = {
    r.copy(regFile1 = r.setBaseReg(RegNames.A, r.getReg(RegNames.DATA8)))
  }

  private def setMemory16fromHL(r: Registers, addr: Int): Registers = {
    val rf1 = r.setBaseReg16(RegNames.DATA16, r.getReg16(RegNames.H))
    r.copy(regFile1 = rf1.copy(data8 = None, wz = Option(addr)))
  }

  private def setMemory16fromIX(r: Registers, addr: Int): Registers

  = {
    val rf1 = r.setBaseReg16(RegNames.DATA16, r.getReg16(RegNames.IX))
    r.copy(regFile1 = rf1.copy(data8 = None, wz = Option(addr)))
  }

  private def setMemory16fromIY(r: Registers, addr: Int): Registers

  = {
    val rf1 = r.setBaseReg16(RegNames.DATA16, r.getReg16(RegNames.IY))
    r.copy(regFile1 = rf1.copy(data8 = None, wz = Option(addr)))
  }

  private def setHLfromMemory16(r: Registers): Registers

  = {
    r.copy(regFile1 = r.setBaseReg16(RegNames.H, r.getReg16(RegNames.DATA16)))
  }

  private def setIXfromMemory16(r: Registers): Registers = {
    r.copy(index = r.setIndexReg(RegNames.IX, r.getReg16(RegNames.DATA16)))
  }

  private def setIYfromMemory16(r: Registers): Registers = {
    r.copy(index = r.setIndexReg(RegNames.IY, r.getReg16(RegNames.DATA16)))
  }

  private def getOverflowFlagAdd(left: Int, right: Int): Boolean = {
    getOverflowFlagAdd(left, right, carry = false)
  }

  /* overflow flag control */
  private def getOverflowFlagAdd(left: Int, right: Int, carry: Boolean): Boolean = {
    var l = left
    var r = right
    if (l > 127) l = l - 256
    if (r > 127) r = r - 256
    l = l + r + (if (carry) 1 else 0)
    (l < -128) || (l > 127)
  }

  private def getOverflowFlagSub(left: Int, right: Int): Boolean = {
    getOverflowFlagSub(left, right, carry = false)
  }

  /* 2's compliment overflow flag control */
  private def getOverflowFlagSub(left: Int, right: Int, carry: Boolean): Boolean = {
    var l = left
    var r = right
    if (l > 127) l = l - 256
    if (r > 127) r = r - 256
    l = l - r - (if (carry) 1 else 0)
    (l < -128) || (l > 127)
  }

  /* 2's compliment overflow flag control */
  private def getOverflowFlagAdd16(left: Int, right: Int, carry: Boolean): Boolean = {
    var l = left
    var r = right
    if (l > 32767)
      l = l - 65536
    if (r > 32767)
      r = r - 65536
    l = l + r + (if (carry) 1 else 0)
    (l < -32768) || (l > 32767)
  }

  /* 2's compliment overflow flag control */
  private def getOverflowFlagSub16(left: Int, right: Int, carry: Boolean): Boolean = {
    var l = left
    var r = right
    if (l > 32767)
      l = l - 65536
    if (r > 32767)
      r = r - 65536
    l = l - r - (if (carry) 1 else 0)
    (l < -32768) || (l > 32767)
  }


  /* half carry flag control */
  private def getHalfCarryFlagAdd(left: Int, right: Int, carry: Boolean) = {
    ((left & 0x0F) + (right & 0x0F) + (if (carry) 1 else 0)) > 0x0F
  }

  /* half carry flag control */
  private def getHalfCarryFlagAdd(left: Int, right: Int) = {
    ((left & 0x0F) + (right & 0x0F)) > 0x0F
  }

  private def getHalfCarryFlagSub(left: Int, right: Int, carry: Boolean): Boolean = {
    (left & 0x0F) < ((right & 0x0F) + (if (carry) 1 else 0))
  }

  private def getHalfCarryFlagSub(left: Int, right: Int): Boolean = {
    (left & 0x0F) < (right & 0x0F)
  }

  /* P/V calculation */
  private def getParityFlag(v: Int): Boolean = {
    val count = nibbleParity(v & 0x0F) + nibbleParity((v & 0xF0) >>> 4)
    0 == (count & 0x01)
  }

  /* Standard condition codes */
  private def cc(y: Int, flags: Registers): Boolean = {
    y match {
      case 0 => flags.isNZ
      case 1 => flags.isZ
      case 2 => flags.isNC
      case 3 => flags.isC
      case 4 => flags.isNPV
      case 5 => flags.isPV
      case 6 => flags.isNS
      case 7 => flags.isS
    }
  }

  // Set S,Z,F5,F3,P from value in a
  private def setFlags_S_Z_F5_F3_P(a: Int, f: Int): Int = {
    var flags = setFlags_S_F5_F3(a, f)
    if (a == 0) flags = flags | 0x40 // Z
    if (getParityFlag(a)) flags = flags | 0x04 // P
    flags
  }

  // Set S,F5,F3 from value in a
  private def setFlags_S_F5_F3(a: Int, f: Int): Int = {
    f | (a & 0xa8) // S,F5,F3
  }

}

