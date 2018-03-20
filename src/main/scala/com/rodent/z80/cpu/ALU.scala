package com.rodent.z80.cpu

import com.rodent.z80.CPUZ._
import com.rodent.z80.func.Utils
import com.rodent.z80.io.{Memory, Ports}

import scala.language.implicitConversions

trait ALU {

  val reg8Bit = Map(0 -> RegNames.B, 1 -> RegNames.C, 2 -> RegNames.D, 3 -> RegNames.E, 4 -> RegNames.H, 5 -> RegNames.L, 6 -> RegNames.DATA8, 7 -> RegNames.A)
  val reg16Bit = Map(0 -> RegNames.B, 1 -> RegNames.D, 2 -> RegNames.H, 3 -> RegNames.SP)
  val reg16Bit2 = Map(0 -> RegNames.B, 1 -> RegNames.D, 2 -> RegNames.H, 3 -> RegNames.A)

  val nibbleParity = Array(0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4)

  val testBit = Array(0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80)

  val resetBit: Array[Int] = testBit.map(x => x ^ 0xFF)

  val memory: Memory
  val ports: Ports

  def execute(regs: Registers): Registers = {
    // http://www.z80.info/decoding.htm
    // http://www.z80.info/z80oplist.txt
    // http://www.z80.info/z80sflag.htm
    // http://clrhome.org/table/
    //    var r = registers
    if (regs.internalRegisters.inst == 0x76) {
      // HALT processing (Interrupt flip flops etc)
      regs
    }
    else {
      if (regs.internalRegisters.single)
      // single byte
        regs.internalRegisters.x match {
          case 0 => general0(regs)
          case 1 => load8(regs)
          case 2 => general8BitALU(regs)
          case 3 => general3(regs)
        }
      else if (regs.internalRegisters.ed) {
        // ed op
        val r = executeED(regs)
        r.copy(internalRegisters = r.internalRegisters.copy(single = true, ed = false))
      }
      else if (regs.internalRegisters.cb)
        if (regs.internalRegisters.dd || regs.internalRegisters.dd) {
          // dd cb nn  op
          val r = executeDDFDCB(regs)
          println("Reset CB DD/FD")
          r.copy(internalRegisters = r.internalRegisters.copy(single = true, cb = false, dd = false, fd = false))
        }
        else {
          // cb op
          var r = executeCB(regs)
          println("Reset CB")
          r.copy(internalRegisters = r.internalRegisters.copy(single = true, cb = false))
        }
      else if (regs.internalRegisters.dd || regs.internalRegisters.fd) {
        // dd nn op or dd op
        var r = executeDDFD(regs)
        if (r.internalRegisters.cb)
          r
        else
          r.copy(internalRegisters = r.internalRegisters.copy(single = true, dd = false, fd = false))
      }
      else
        throw new UndefOpcode("Addr: " + Utils.toHex16(regs.getPC) + " inst : " + Utils.toHex8(regs.internalRegisters.inst))
    }
  }


  // Instructions 0x00 -> 0x3F
  // Instructions 0x00 -> 0x3F
  // Instructions 0x00 -> 0x3F

  private def general0(registers: Registers): Registers = {
    var r = registers
    r.internalRegisters.z match {
      case 0 => jmpRelative(r) // flags ok
      case 1 => ldadd16(r) // flags ok
      case 2 => indirectLoad(r) // flags ok
      case 3 => incDec16(r) // flags ok
      case 4 => inc8(r) // flags ok
      case 5 => dec8(r) // flags ok
      case 6 => ldImmediate8(r) // flags ok
      case 7 => variousBlock0(r) // flags ok
    }
  }

  // Instructions 0x80 -> 0xBF
  // Instructions 0x80 -> 0xBF
  // Instructions 0x80 -> 0xBF

  private def general8BitALU(registers: Registers): Registers = {
    var r = registers
    r.internalRegisters.y match {
      case 0 => addAdc8(r, adc = false) // flags ok
      case 1 => addAdc8(r, adc = true) // flags ok
      case 2 => subSbc8(r, sbc = false) // flags ok
      case 3 => subSbc8(r, sbc = true) // flags ok
      case 4 => andOrXor8(r, r.internalRegisters.y, h = true, (l: Int, r: Int) => l & r) // flags ok
      case 5 => andOrXor8(r, r.internalRegisters.y, h = false, (l: Int, r: Int) => l ^ r) // flags ok
      case 6 => andOrXor8(r, r.internalRegisters.y, h = false, (l: Int, r: Int) => l | r) // flags ok
      case 7 => cp8(r)
    }
  }

  // Instructions 0xC0 -> 0xFF
  // Instructions 0xC0 -> 0xFF
  // Instructions 0xC0 -> 0xFF

  private def general3(registers: Registers): Registers = {
    var r = registers
    r.internalRegisters.z match {
      case 0 => retcc(r) // flags ok
      case 1 => variousExPop(r) // flags ok
      case 2 => jpcc(r) // flags ok
      case 3 => variousJpExInOut(r) // flags ok
      case 4 => callcc(r) // flags ok
      case 5 => variousPushCallSpecial(r) // flags ok
      case 6 => general8BitALU(r) // flags ok
      case 7 => rst(r) // flags ok
    }
  }

  // various block 3, ex, pop etc
  private def variousExPop(r: Registers): Registers = {
    if (0 == r.internalRegisters.q) {
      // pop
      var rf1 = r.setBaseReg16(reg16Bit2(r.internalRegisters.p), memory.pop(r.getSP))
      val cr = r.setSP((r.getSP + 2).limit16)
      r.copy(regFile1 = rf1, controlRegisters = cr)
    }
    else
      r.internalRegisters.p match {
        case 0 => ret(r)
        case 1 => r.copy(regFile1 = r.regFile2, regFile2 = r.regFile1) // exx
        case 2 => r.copy(controlRegisters = r.setPC(r.getReg16(RegNames.H))) // jp HL
        case 3 => r.copy(controlRegisters = r.setSP(r.getReg16(RegNames.H))) // ld sp,hl
      }
  }

  // various block 3, jp,ex,in,out
  private def variousJpExInOut(r: Registers): Registers = {
    r.internalRegisters.y match {
      case 0 => jp(r) // jp nn
      case 1 => {
        println("Set CB")
        r.copy(internalRegisters = r.internalRegisters.copy(single = false, cb = true))
      }
      case 2 => ports.setPort(r.regFile1.data8.get, r.getA)
        r
      case 3 => r.copy(regFile1 = r.regFile1.copy(a = 0, f = 0)) // xyzzy
      case 4 => // ex (sp),hl
        val temp = memory.pop(r.getSP)
        memory.push((r.getSP + 2).limit16, r.getReg16(RegNames.H))
        r.copy(regFile1 = r.setBaseReg16(RegNames.H, temp))
      case 5 => r.copy(regFile1 = r.regFile1.copy(h = r.regFile1.d, l = r.regFile1.e, d = r.regFile1.h, e = r.regFile1.l)) // ex hl,de
      case 6 => r // DI
      case 7 => r // EI
    }
  }

  // various block 3, pushCallSpecial
  private def variousPushCallSpecial(r: Registers): Registers = {
    if (0 == r.internalRegisters.q) {
      // push
      memory.push(r.getSP, r.getReg16(reg16Bit2(r.internalRegisters.p)))
      val cr = r.setSP((r.getSP - 2).limit16)
      r.copy(controlRegisters = cr)
    }
    else
      r.internalRegisters.p match {
        case 0 => call(r)
        case 1 => r.copy(internalRegisters = r.internalRegisters.copy(single = false, dd = true))
        case 2 => r.copy(internalRegisters = r.internalRegisters.copy(single = false, ed = true))
        case 3 => r.copy(internalRegisters = r.internalRegisters.copy(single = false, fd = true))
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
    if (0 == r.internalRegisters.q)
      r.internalRegisters.p match {
        case 0 => setMemory8fromA(r, r.getReg16(RegNames.B)) // LD (BC), A
        case 1 => setMemory8fromA(r, r.getReg16(RegNames.D)) // LD (DE), A
        case 2 if r.internalRegisters.dd => setMemory16fromIX(r, r.getReg16(RegNames.DATA16)) // LD (nn), IX
        case 2 if r.internalRegisters.fd => setMemory16fromIY(r, r.getReg16(RegNames.DATA16)) // LD (nn), IY
        case 2 => setMemory16fromHL(r, r.getReg16(RegNames.DATA16)) // LD (nn), HL
        case 3 => setMemory8fromA(r, r.getReg16(RegNames.DATA16)) // LD (nn), A
      }
    else
      r.internalRegisters.p match {
        case 0 => setAfromMemory8(r) // LD A, (BC)
        case 1 => setAfromMemory8(r) // LD A, (DE)
        case 2 if r.internalRegisters.dd => setIXfromMemory16(r) // LD IX, (nn)
        case 2 if r.internalRegisters.fd => setIYfromMemory16(r) // LD IY, (nn)
        case 2 => setHLfromMemory16(r) // LD HL, (nn)
        case 3 => setAfromMemory8(r) // LD A, (nn)
      }
  }

  // various block 0, 8 bit math ops
  private def variousBlock0(r: Registers): Registers = {
    r.internalRegisters.y match {
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
    val c = (r.getA & 0x80) > 0
    var v = (r.getA << 1).limit8
    if (c) v += 1
    r.copy(regFile1 = r.setResultA(v, cf = Some(c), f5f = v.f5, f3f = v.f3, hf = Some(false), nf = Some(false)))
  }

  // rrca
  private def rrca(r: Registers): Registers = {
    val c = (r.getA & 0x01) > 0
    var v = r.getA >>> 1
    if (c) v = v | 0x80
    r.copy(regFile1 = r.setResultA(v, cf = Some(c), f5f = v.f5, f3f = v.f3, hf = Some(false), nf = Some(false)))
  }

  // rla
  private def rla(r: Registers): Registers = {
    val c = (r.getA & 0x80) > 0
    var a = (r.getA << 1).limit8
    if (r.isC) a += 1
    r.copy(regFile1 = r.setResultA(a, cf = Some(c), f5f = a.f5, f3f = a.f3, hf = Some(false), nf = Some(false)))
  }

  // rra
  private def rra(r: Registers): Registers = {
    val c = (r.getA & 0x01) > 0
    var a = r.getA >>> 1
    if (r.isC) a += 1
    r.copy(regFile1 = r.setResultA(a, cf = Some(c), f5f = a.f5, f3f = a.f3, hf = Some(false), nf = Some(false)))
  }

  // cpl
  private def cpl(r: Registers): Registers = {
    val a = r.getA ^ 0xFF
    r.copy(regFile1 = r.setResultA(a, f5f = a.f5, f3f = a.f3, hf = Some(true), nf = Some(true)))
  }

  // scf
  private def scf(r: Registers): Registers = {
    val a = r.getA
    r.copy(regFile1 = r.setResultA(a, f5f = a.f5, f3f = a.f3, hf = Some(false), nf = Some(false), cf = Some(true)))
  }

  // ccf
  private def ccf(r: Registers): Registers = {
    val a = r.getA
    r.copy(regFile1 = r.setResultA(a, f5f = a.f5, f3f = a.f3, hf = Some(r.isC), nf = Some(false), cf = Some(!r.isC)))
  }

  // DAA is weird, can't find Zilog algorithm so using +0110 if Nibble>9 algorithm.
  private def daa(r: Registers): Registers = {
    var a = r.getA
    var incr = 0
    if (r.isH || ((a & 0x00F) > 0x09)) incr = 0x06
    if (r.isC || (a > 0x9f) || ((a > 0x8f) && ((a & 0x0f) > 0x09))) incr = incr | 0x60
    var carry = r.isC
    if (a > 0x99) carry = true
    //
    if (r.isN) {
      // sub
      val raw = r.getA - incr
      val v = raw.limit8
      //
      val s = (raw & 0x80) != 0
      val z = v == 0
      val h = getHalfCarryFlagSub(r.getA, incr, carry = false)
      val pv = getOverflowFlagSub(r.getA, incr, carry = false)
      val n = true
      val c = 0 != (raw & 0xFF)
      //
      r.copy(regFile1 = r.setResultA(a, cf = Some(carry), pvf = Some(getParityFlag(v))))
    }
    else {
      // add
      val raw = r.getA + incr
      val v = raw.limit8
      //
      val s = (raw & 0x80) != 0
      val z = v == 0
      val h = getHalfCarryFlagAdd(r.getA, incr, carry = false)
      val pv = getOverflowFlagAdd(r.getA, incr, carry = false)
      val n = false
      val c = 0 != (raw & 0xFF)
      //
      r.copy(regFile1 = r.setResultA(a, sf = Some(s), zf = Some(z), hf = Some(h), nf = Some(n), f3f = v.f3, f5f = v.f5, cf = Some(carry), pvf = Some(getParityFlag(v))))
    }
  }

  // ld registers,nn
  private def ldImmediate8(r: Registers): Registers = {
    val regName = reg8Bit(r.internalRegisters.y)
    r.copy(regFile1 = r.setBaseReg(regName, r.getReg(RegNames.DATA8)))
  }

  // inc / dec rr
  private def incDec16(r: Registers): Registers = {
    var rf1 = r.regFile1
    val regName = reg16Bit(r.p)
    var v = r.getReg16(regName)
    v = if (0 == r.q) v.inc16 else v.dec16
    if (regName == RegNames.SP)
      r.copy(controlRegisters = r.setControlReg(regName, v))
    else if (r.internalRegisters.dd)
      r.copy(indexRegisters = r.setIndexReg(RegNames.IX, v))
    else if (r.internalRegisters.fd)
      r.copy(indexRegisters = r.setIndexReg(RegNames.IY, v))
    else
      r.copy(regFile1 = r.setBaseReg16(regName, v))
  }

  // inc registers
  private def inc8(registers: Registers): Registers = {
    val regName = reg8Bit(registers.internalRegisters.y)
    val v = registers.getReg(regName).inc8
    //
    val s = (v & 0x80) != 0
    val z = v == 0
    val h = (v & 0x0F) == 0
    val pv = v == 0x80
    val n = false
    //
    registers.copy(regFile1 = registers.setResult8(regName, v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(h), f3f = v.f3,
      pvf = Some(pv), nf = Some(n)))
  }

  // dec registers
  private def dec8(registers: Registers): Registers = {
    val regName = reg8Bit(registers.internalRegisters.y)
    val v = registers.getReg(regName).dec8
    //
    val s = (v & 0x80) != 0
    val z = v == 0
    val h = (v & 0x0F) == 0x0F
    val pv = v == 0x80
    val n = true
    //
    registers.copy(regFile1 = registers.setResult8(regName, v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(h), f3f = v.f3,
      pvf = Some(pv), nf = Some(n)))
  }

  // LD / ADD 16
  private def ldadd16(registers: Registers): Registers = {
    var r = registers.regFile1
    var cr = registers.controlRegisters
    if (registers.internalRegisters.q == 0) {
      // LD rr,(mm)
      val v = r.data16.get
      val lsb = v.lsb
      val msb = v.msb
      registers.internalRegisters.p match {
        case 0 => r = r.copy(b = msb, c = lsb)
        case 1 => r = r.copy(d = msb, e = lsb)
        case 2 if registers.internalRegisters.dd => registers.copy(indexRegisters = registers.setResultIX(v))
        case 2 if registers.internalRegisters.fd => registers.copy(indexRegisters = registers.setResultIY(v))
        case 2 => r = r.copy(h = msb, l = lsb)
        case 3 => cr = cr.copy(sp = v)
      }
      registers.copy(regFile1 = r, controlRegisters = cr)
    } else {
      // ADD HL/IX/IY,rr
      val hl = registers.getReg16Index(RegNames.H)
      val rr = registers.getReg16Index(reg16Bit(registers.internalRegisters.p))
      val v = hl + rr
      // sort out flags
      val h = (((hl & 0x0FFF) + (rr & 0x0FFF)) & 0xF000) != 0 // half carry in msb
      val f5 = (v & 0x2000) != 0
      val f3 = (v & 0x0800) != 0
      val c = v > 0xFFFF
      //
      val regFile = registers.setFlags(f5f = Some(f5), hf = Some(h), f3f = Some(f3), nf = Some(false), cf = Some(c))
      if (registers.dd)
        registers.copy(indexRegisters = registers.setResultIX(v), regFile1 = regFile)
      else if (registers.fd)
        registers.copy(indexRegisters = registers.setResultIY(v), regFile1 = regFile)
      else
        registers.copy(regFile1 = regFile.copy(h = v.msb, l = v.lsb))
    }
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
      case 6 => r = r.copy(data8 = Option(v))
      case 7 => r = r.copy(a = v)
    }
    registers.copy(regFile1 = r)
  }

  // standard 8 bit add/adc src,dst instrucitons
  private def addAdc8(registers: Registers, adc: Boolean): Registers = {
    val srcReg = registers.internalRegisters.z
    val src = registers.getReg(reg8Bit(srcReg))
    val carry = adc && registers.isC
    val raw = registers.getA + src + (if (carry) 1 else 0)
    val v = raw.limit8
    //
    val s = (raw & 0x80) != 0
    val z = v == 0
    val h = getHalfCarryFlagAdd(registers.getA, src, carry)
    val pv = getOverflowFlagAdd(registers.getA, src, carry)
    val n = false
    val c = raw > 0xFF
    //
    registers.copy(regFile1 = registers.setResultA(v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(h), f3f = v.f3, pvf = Some(pv), nf = Some(n), cf = Some(c))
    )
  }

  // standard 8 bit sub/sbc src,dst instrucitons
  private def subSbc8(registers: Registers, sbc: Boolean): Registers = {
    val srcReg = registers.internalRegisters.z
    val src = registers.getReg(reg8Bit(srcReg))
    val carry = sbc && registers.isC
    val raw = registers.getA - src - (if (carry) 1 else 0)
    val v = raw.limit8
    //
    val s = (raw & 0x80) != 0
    val z = v == 0
    val h = getHalfCarryFlagSub(registers.getA, src, carry)
    val pv = getOverflowFlagSub(registers.getA, src, carry)
    val n = true
    val c = raw > 0xFF
    //
    registers.copy(regFile1 = registers.setResultA(v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(h), f3f = v.f3, pvf = Some(pv), nf = Some(n), cf = Some(c))
    )
  }

  // standard 8 bit cp src,dst instrucitons
  private def cp8(registers: Registers): Registers = {
    val srcReg = registers.internalRegisters.z
    val src = registers.getReg(reg8Bit(srcReg))
    val raw = registers.getA - src
    val v = raw.limit8
    //
    val s = (raw & 0x80) != 0
    val z = v == 0
    val h = getHalfCarryFlagSub(registers.getA, src, carry = false)
    val pv = getOverflowFlagSub(registers.getA, src, carry = false)
    val n = true
    val c = raw > 0xFF
    //
    registers.copy(regFile1 = registers.setFlags(sf = Some(s), zf = Some(z), f5f = src.f5, hf = Some(h), f3f = src.f3, pvf = Some(pv), nf = Some(n), cf = Some(c))
    )
  }

  // standard 8 bit and src,dst instrucitons
  private def andOrXor8(registers: Registers, y: Int, h: Boolean, f: (Int, Int) => Int): Registers = {
    val srcReg = registers.internalRegisters.z
    val src = registers.getReg(reg8Bit(srcReg))
    val v = f(registers.getA, src)
    //
    val s = (v & 0x80) != 0
    val z = v == 0
    val pv = getParityFlag(v)
    val n = false
    val c = false
    //
    registers.copy(regFile1 = registers.setResultA(v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(h), f3f = v.f3, pvf = Some(pv), nf = Some(n), cf = Some(c))
    )
  }

  // ret cc
  private def retcc(registers: Registers): Registers = {
    if (cc(registers.internalRegisters.y, registers)) {
      ret(registers)
    }
    else
      registers
  }

  // ret
  private def ret(registers: Registers): Registers = {
    //   println("return sp:" + Utils.toHex16(registers.getSP))
    val sp = registers.getSP
    val addr = memory.pop(sp)
    registers.copy(controlRegisters = registers.controlRegisters.copy(pc = addr, sp = (sp + 2).limit16))
  }

  // jp cc
  private def jpcc(registers: Registers): Registers = {
    if (cc(registers.internalRegisters.y, registers))
      jp(registers)
    else
      registers
  }

  // call cc
  private def callcc(registers: Registers): Registers = {
    if (cc(registers.internalRegisters.y, registers))
      call(registers)
    else
      registers
  }

  // rst
  private def rst(registers: Registers): Registers = {
    val addr = registers.internalRegisters.y * 8
    memory.push(registers.getSP, registers.getPC)
    registers.copy(controlRegisters = registers.controlRegisters.copy(pc = addr, sp = (registers.getSP - 2).limit16))
  }

  // relative jump
  private def jr(r: Registers): Registers = {
    var offset = r.getReg(RegNames.DATA8)
    if (offset > 0x007F) offset = offset - 0x0100
    val pc = (r.getPC + offset).limit16
    r.copy(controlRegisters = r.setPC(pc))
  }

  // absolute jump
  private def jp(r: Registers): Registers = {
    r.copy(controlRegisters = r.setPC(r.getReg(RegNames.DATA16)))
  }

  // absolute call
  private def call(r: Registers): Registers = {
    //   println("call from " + Utils.toHex16(r.getPC) + " to " + Utils.toHex16(r.getReg(RegNames.M16)) + " sp:" + Utils.toHex16(r.getSP))
    memory.push(r.getSP, r.getPC)
    r.copy(controlRegisters = r.controlRegisters.copy(pc = r.getReg(RegNames.DATA16), sp = (r.getSP - 2).limit16))
  }

  // Execute CB prefix
  // Execute CB prefix
  // Execute CB prefix

  private def executeCB(regs: Registers): Registers = {
    val r = regs.copy(internalRegisters = regs.internalRegisters.copy(cb = false))
    r.internalRegisters.x match {
      case 0 => // rot[y] r[z]
        r.internalRegisters.y match {
          case 0 => // rlc
            val reg = reg8Bit(r.internalRegisters.z)
            var v = r.getReg(reg) << 1
            val c = (v & 0xFF00) != 0
            if (c) v = v | 0x01
            v = v.limit8
            val s = (v & 0x80) != 0
            val z = v == 0
            val pv = this.getParityFlag(v)
            r.copy(regFile1 = r.setResult8(reg, v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(false), f3f = v.f3, pvf = Some(pv), nf = Some(false), cf = Some(c)))
          case 1 => // rrc
            val reg = reg8Bit(r.internalRegisters.z)
            var v = r.getReg(reg)
            val c = (v & 0x01) != 0
            v = v >>> 1
            if (c) v = v | 0x80
            val s = c
            val z = v == 0
            val pv = this.getParityFlag(v)
            r.copy(regFile1 = r.setResult8(reg, v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(false), f3f = v.f3, pvf = Some(pv), nf = Some(false), cf = Some(c)))
          case 2 => // rl
            val reg = reg8Bit(r.internalRegisters.z)
            var v = r.getReg(reg) << 1
            if (r.isC) v = v | 0x01
            val c = (v & 0xFF00) != 0
            v = v.limit8
            val s = (v & 0x80) != 0
            val z = v == 0
            val pv = this.getParityFlag(v)
            r.copy(regFile1 = r.setResult8(reg, v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(false), f3f = v.f3, pvf = Some(pv), nf = Some(false), cf = Some(c)))
          case 3 => // rr
            val reg = reg8Bit(r.internalRegisters.z)
            var v = r.getReg(reg)
            val c = (v & 0x01) != 0
            v = v >>> 1
            if (r.isC) v = v | 0x80
            val s = c
            val z = v == 0
            val pv = this.getParityFlag(v)
            r.copy(regFile1 = r.setResult8(reg, v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(false), f3f = v.f3, pvf = Some(pv), nf = Some(false), cf = Some(c)))
          case 4 => // sla
            val reg = reg8Bit(r.internalRegisters.z)
            var v = r.getReg(reg) << 1
            val c = (v & 0xFF00) != 0
            v = v.limit8
            val s = (v & 0x80) != 0
            val z = v == 0
            val pv = this.getParityFlag(v)
            r.copy(regFile1 = r.setResult8(reg, v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(false), f3f = v.f3, pvf = Some(pv), nf = Some(false), cf = Some(c)))
          case 5 => // sra
            val reg = reg8Bit(r.internalRegisters.z)
            var v = r.getReg(reg)
            val c = (v & 0x01) != 0
            val s = (v & 0x80) != 0
            v = v >>> 1
            if (s) v = v | 0x80
            val z = v == 0
            val pv = this.getParityFlag(v)
            r.copy(regFile1 = r.setResult8(reg, v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(false), f3f = v.f3, pvf = Some(pv), nf = Some(false), cf = Some(c)))
          case 6 => // sll
            val reg = reg8Bit(r.internalRegisters.z)
            var v = (r.getReg(reg) << 1) | 0x01
            val c = (v & 0xFF00) != 0
            v = v.limit8
            val s = (v & 0x80) != 0
            val z = false
            val pv = this.getParityFlag(v)
            r.copy(regFile1 = r.setResult8(reg, v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(false), f3f = v.f3, pvf = Some(pv), nf = Some(false), cf = Some(c)))
          case 7 => // srl
            val reg = reg8Bit(r.internalRegisters.z)
            var v = r.getReg(reg)
            val c = (v & 0x01) != 0
            v = v >>> 1
            val s = false
            val z = v == 0
            val pv = this.getParityFlag(v)
            r.copy(regFile1 = r.setResult8(reg, v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(false), f3f = v.f3, pvf = Some(pv), nf = Some(false), cf = Some(c)))
        }
      case 1 => // BIT y, r[z]
        val v = r.getReg(reg8Bit(r.internalRegisters.z))
        val z = 0 == (v & testBit(r.internalRegisters.y))
        var s = false
        if (7 == r.internalRegisters.y) s = 0 != (v & 0x80)
        r.copy(regFile1 = r.setFlags(sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(true), f3f = v.f3, pvf = Some(z), nf = Some(false)))
      case 2 => // RES y, r[z]
        val reg = reg8Bit(r.internalRegisters.z)
        val v = r.getReg(reg) & resetBit(r.internalRegisters.y)
        val z = 0 == v
        var s = false
        if (7 == r.internalRegisters.y) s = 0 != (v & 0x80)
        r.copy(regFile1 = r.setResult8(reg, v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(true), f3f = v.f3, pvf = Some(z), nf = Some(false)))
      case 3 => // SET y, r[z]
        val reg = reg8Bit(r.internalRegisters.z)
        val v = r.getReg(reg) | testBit(r.internalRegisters.y)
        val z = 0 == v
        var s = false
        if (7 == r.internalRegisters.y) s = 0 != (v & 0x80)
        r.copy(regFile1 = r.setResult8(reg, v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(true), f3f = v.f3, pvf = Some(z), nf = Some(false)))
    }
  }

  // Execute DD FD prefix
  // Execute DD FD prefix
  // Execute DD FD prefix

  private def executeDDFD(r: Registers): Registers = {
    r.internalRegisters.inst match {
      case 0x09 => ldadd16(r)
      case 0x19 => ldadd16(r)
      case 0x21 => ldadd16(r)
      case 0x22 => indirectLoad(r)
      case 0x23 => incDec16(r)
      case 0x29 => ldadd16(r)
      case 0x2A => indirectLoad(r)
      case 0x2B => incDec16(r)
      case 0x34 => inc8(r)
      case 0x35 => dec8(r)
      case 0x36 => ldImmediate8(r)
      case 0x39 => ldadd16(r)
      // ld r,(ixiy+dd)
      case 0x46 => load8(r) // ld b,(ixiy+dd)
      case 0x4E => load8(r) // ld c
      case 0x56 => load8(r) // ld d
      case 0x5E => load8(r) // ld e
      case 0x66 => load8(r) // ld h
      case 0x6E => load8(r) // ld l
      case 0x7E => load8(r) // ld a
      // ld (ixiy+dd),r
      case 0x70 => load8(r) // ld (ixiy+dd),b
      case 0x71 => load8(r) // ld c
      case 0x72 => load8(r) // ld d
      case 0x73 => load8(r) // ld e
      case 0x74 => load8(r) // ld h
      case 0x75 => load8(r) // ld l
      case 0x77 => load8(r) // ld a
      // add/adc/sub/sbc/and/xor/or/cp
      case 0x86 => addAdc8(r, adc = false)
      case 0x8E => addAdc8(r, adc = true)
      case 0x96 => subSbc8(r, sbc = false)
      case 0x9E => subSbc8(r, sbc = true)
      case 0xA6 => andOrXor8(r, r.internalRegisters.y, h = true, (l: Int, r: Int) => l & r)
      case 0xA6 => andOrXor8(r, r.internalRegisters.y, h = true, (l: Int, r: Int) => l & r)
      case 0xAE => andOrXor8(r, r.internalRegisters.y, h = false, (l: Int, r: Int) => l | r)
      case 0xB6 => andOrXor8(r, r.internalRegisters.y, h = false, (l: Int, r: Int) => l ^ r)
      case 0xBE => cp8(r)
      case 0xCB => {
        println("Set CB DD/FD")
        r.copy(internalRegisters = r.internalRegisters.copy(single = false, cb = true))
      }
      //
      case 0xE1 => //pop
        var v = memory.pop(r.getSP)
        val cr = r.setSP((r.getSP + 2).limit16)
        if (r.internalRegisters.dd)
          r.copy(controlRegisters = cr, indexRegisters = r.indexRegisters.copy(ix = v))
        else
          r.copy(controlRegisters = cr, indexRegisters = r.indexRegisters.copy(iy = v))
      //
      case 0xE3 => // ex (sp),ix
        val v = memory.pop(r.getSP)
        memory.push((r.getSP + 2).limit16, r.getReg16(RegNames.H))
        if (r.internalRegisters.dd)
          r.copy(indexRegisters = r.indexRegisters.copy(ix = v))
        else
          r.copy(indexRegisters = r.indexRegisters.copy(iy = v))
      //
      case 0xE5 => // push
        if (r.internalRegisters.dd)
          memory.push(r.getSP, r.getReg16(RegNames.IX))
        else
          memory.push(r.getSP, r.getReg16(RegNames.IY))
        val cr = r.setSP((r.getSP - 2).limit16)
        r.copy(controlRegisters = cr)
      //
      case 0xE9 => // jp (ixiy)
        if (r.internalRegisters.dd)
          r.copy(controlRegisters = r.setPC(r.getReg16(RegNames.IX)))
        else
          r.copy(controlRegisters = r.setPC(r.getReg16(RegNames.IY)))
      //
      case 0xF9 => // ld sp,ix
        if (r.internalRegisters.dd)
          r.copy(controlRegisters = r.setSP(r.getReg16(RegNames.IX)))
        else
          r.copy(controlRegisters = r.setSP(r.getReg16(RegNames.IY)))
      //
      case _ => throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (ddfd): " + Utils.toHex8(r.internalRegisters.inst))
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
    var f5f3 = (r.getA + r.getReg(RegNames.DATA8)).limit8 & 0x28 // f5,f33
    flags = flags | f5f3
    if (bc != 0) flags = flags | 0x04
    r.regFile1.copy(f = flags, h = hl.msb, l = hl.lsb, d = de.msb, e = de.lsb, b = bc.msb, c = bc.lsb, wz = Option(target))
  }


  private def ldd(r: Registers): BaseRegisters = {
    val hl = r.getReg16(RegNames.H).dec16
    val target = r.getReg16(RegNames.D)
    val de = target.dec16
    val bc = r.getReg16(RegNames.B).dec16
    var flags = r.getReg(RegNames.F) & 0xC1 // f5=0,h=0,f3=0,pv=0,n=0
    var f5f3 = (r.getA + r.getReg(RegNames.DATA8)).limit8 & 0x28 // f5,f33
    flags = flags | f5f3
    if (bc != 0) flags = flags | 0x04
    r.regFile1.copy(f = flags, h = hl.msb, l = hl.lsb, d = de.msb, e = de.lsb, b = bc.msb, c = bc.lsb, wz = Option(target))
  }

  private def cpi(r: Registers): BaseRegisters = {
    val hl = r.getReg16(RegNames.H).inc16
    val bc = r.getReg16(RegNames.B).dec16
    var flags = (r.getReg(RegNames.F) & 0x01) | 0x02 // c = unchanged, n=1
    //
    val raw = r.getA - r.getReg(RegNames.DATA8)
    var v = raw.limit8
    val s = (raw & 0x80) != 0
    val z = v == 0
    val h = getHalfCarryFlagSub(r.getA, r.getReg(RegNames.DATA8), carry = false)
    //
    if (h) v -= 1
    var f5f3 = v & 0x28 // f5,f3
    flags = flags | f5f3
    if (bc != 0) flags = flags | 0x04
    r.regFile1.copy(f = flags, h = hl.msb, l = hl.lsb, b = bc.msb, c = bc.lsb)
  }


  private def cpd(r: Registers): BaseRegisters = {
    val hl = r.getReg16(RegNames.H).dec16
    val bc = r.getReg16(RegNames.B).dec16
    var flags = (r.getReg(RegNames.F) & 0x01) | 0x02 // c = unchanged, n=1
    //
    val raw = r.getA - r.getReg(RegNames.DATA8)
    var v = raw.limit8
    val s = (raw & 0x80) != 0
    val z = v == 0
    val h = getHalfCarryFlagSub(r.getA, r.getReg(RegNames.DATA8), carry = false)
    //
    if (h) v -= 1
    var f5f3 = v & 0x28 // f5,f3
    flags = flags | f5f3
    if (bc != 0) flags = flags | 0x04
    r.regFile1.copy(f = flags, h = hl.msb, l = hl.lsb, b = bc.msb, c = bc.lsb)
  }

  private def executeED(r: Registers): Registers = {
    if (1 == r.internalRegisters.x)
      executeEDBlock1(r)
    else
      r.internalRegisters.inst match {
        case 0xA0 => r.copy(regFile1 = ldi(r)) // LDI
        case 0xA1 => r.copy(regFile1 = cpi(r)) // CPI
        case 0xA8 => r.copy(regFile1 = ldd(r)) // LDD
        case 0xA9 => r.copy(regFile1 = cpd(r)) // CPD
        case 0xB0 => { // LDIR
          val rf1 = ldi(r)
          if ((0 != rf1.b) || (0 != rf1.c))
            r.copy(regFile1 = rf1, controlRegisters = r.controlRegisters.copy(pc = r.controlRegisters.pc.dec16.dec16))
          else
            r.copy(regFile1 = rf1)
        }
        case 0xB1 => { // CPIR
          val rf1 = cpi(r)
          if ((0 != rf1.b) || (0 != rf1.c))
            r.copy(regFile1 = rf1, controlRegisters = r.controlRegisters.copy(pc = r.controlRegisters.pc.dec16.dec16))
          else
            r.copy(regFile1 = rf1)
        }
        case 0xB8 => { // LDDR
          val rf1 = ldd(r)
          if ((0 != rf1.b) || (0 != rf1.c))
            r.copy(regFile1 = rf1, controlRegisters = r.controlRegisters.copy(pc = r.controlRegisters.pc.dec16.dec16))
          else
            r.copy(regFile1 = rf1)
        }
        case 0xB9 => { // CPDR
          val rf1 = cpd(r)
          if ((0 != rf1.b) || (0 != rf1.c))
            r.copy(regFile1 = rf1, controlRegisters = r.controlRegisters.copy(pc = r.controlRegisters.pc.dec16.dec16))
          else
            r.copy(regFile1 = rf1)
        }
        //
        case _ => throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (ed): " + Utils.toHex8(r.internalRegisters.inst))
      }
  }

  private def executeEDBlock1(r: Registers): Registers = {
    r.internalRegisters.z match {
      case 3 => //
        if (r.internalRegisters.q == 0) {
          memory.setMemory16(r.getReg16(RegNames.DATA16), r.getReg16(reg16Bit(r.internalRegisters.p)))
          r
        }
        else {
          val v = r.getReg16(RegNames.DATA16)
          if (r.internalRegisters.p == 3) // sp
            r.copy(controlRegisters = r.setSP(v))
          else
            r.copy(regFile1 = r.setBaseReg16(reg16Bit(r.internalRegisters.p), v))
        }
      //
      case _ => throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (ed): " + Utils.toHex8(r.internalRegisters.inst))
    }
  }


  // Execute DD FD CB prefix
  // Execute DD FD CB prefix
  // Execute DD FD CB prefix

  private def executeDDFDCB(r: Registers): Registers = {
    println("executeDDFDCB " + Utils.toHex8(r.getInst))
    r.internalRegisters.x match {
      case 0 => rotateDDFDCB(r)
      case 1 => bitDDFDCB(r)
      case 2 => resDDFDCB(r)
      case 3 => setDDFDCB(r)
      case _ => throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (dd cb): " + Utils.toHex8(r.internalRegisters.inst))
    }
  }

  private def rotateDDFDCB(r: Registers): Registers = {
    r.internalRegisters.z match {
      //  case 1 => r
      case _ => throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (dd cb): " + Utils.toHex8(r.internalRegisters.inst))
    }
  }

  private def bitDDFDCB(r: Registers): Registers = {
    if (r.internalRegisters.z == 6) {
      val v = r.getReg(RegNames.DATA8)
      val z = 0 == (v & testBit(r.internalRegisters.y))
      var s = false
      if (7 == r.internalRegisters.y) s = 0 != (v & 0x80)
      r.copy(regFile1 = r.setFlags(sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(true), f3f = v.f3, pvf = Some(z), nf = Some(false)))
    }
    else
      throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (dd cb): " + Utils.toHex8(r.internalRegisters.inst))
  }

  private def resDDFDCB(r: Registers): Registers = {
    throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (dd cb): " + Utils.toHex8(r.internalRegisters.inst))
  }

  private def setDDFDCB(r: Registers): Registers = {
    if (r.internalRegisters.z == 6) {
      val v = r.getReg(RegNames.DATA8) | testBit(r.internalRegisters.y)
      val z = 0 == v
      var s = false
      if (7 == r.internalRegisters.y) s = 0 != (v & 0x80)
      r.copy(regFile1 = r.setResult8(RegNames.DATA8, v, sf = Some(s), zf = Some(z), f5f = v.f5, hf = Some(true), f3f = v.f3, pvf = Some(z), nf = Some(false)))
    }
    else
      throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " inst (dd cb): " + Utils.toHex8(r.internalRegisters.inst))
  }


  // helpers - helpers - helpers
  // helpers - helpers - helpers
  // helpers - helpers - helpers

  private def setMemory8fromA(r: Registers, addr: Int): Registers = {
    val rf1 = r.setBaseReg(RegNames.DATA8, r.getReg(RegNames.A))
    r.copy(regFile1 = rf1.copy(wz = Option(addr)))
  }

  private def setAfromMemory8(r: Registers): Registers = {
    r.copy(regFile1 = r.setBaseReg(RegNames.A, r.getReg(RegNames.DATA8)))
  }

  private def setMemory16fromHL(r: Registers, addr: Int): Registers = {
    val rf1 = r.setBaseReg16(RegNames.DATA16, r.getReg16(RegNames.H))
    r.copy(regFile1 = rf1.copy(wz = Option(addr)))
  }

  private def setMemory16fromIX(r: Registers, addr: Int): Registers = {
    val rf1 = r.setBaseReg16(RegNames.DATA16, r.getReg16(RegNames.IX))
    r.copy(regFile1 = rf1.copy(wz = Option(addr)))
  }

  private def setMemory16fromIY(r: Registers, addr: Int): Registers = {
    val rf1 = r.setBaseReg16(RegNames.DATA16, r.getReg16(RegNames.IY))
    r.copy(regFile1 = rf1.copy(wz = Option(addr)))
  }

  private def setHLfromMemory16(r: Registers): Registers = {
    r.copy(regFile1 = r.setBaseReg16(RegNames.H, r.getReg16(RegNames.DATA16)))
  }

  private def setIXfromMemory16(r: Registers): Registers = {
    r.copy(indexRegisters = r.setIndexReg(RegNames.IX, r.getReg16(RegNames.DATA16)))
  }

  private def setIYfromMemory16(r: Registers): Registers = {
    r.copy(indexRegisters = r.setIndexReg(RegNames.IY, r.getReg16(RegNames.DATA16)))
  }

  private def getOverflowFlagAdd(left: Int, right: Int): Boolean = {
    getOverflowFlagAdd(left, right, carry = false)
  }

  /* pverflow flag control */
  private def getOverflowFlagAdd(left: Int, right: Int, carry: Boolean): Boolean = {
    var l = left
    var r = right
    if (l > 127) l = l - 256
    if (r > 127) r = r - 256
    l = l + r
    if (carry) l += 1
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
    l = l - r
    if (carry) l -= 1
    (l < -128) || (l > 127)
  }

  /* half carry flag control */
  private def getHalfCarryFlagAdd(left: Int, right: Int, carry: Boolean) = {
    (left & 0x0F) + (right & 0x0F) + (if (carry) 1 else 0) > 0x0F
  }

  private def getHalfCarryFlagSub(left: Int, right: Int, carry: Boolean): Boolean = {
    (left & 0x0F) < ((right & 0x0F) + (if (carry) 1 else 0))
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
}

