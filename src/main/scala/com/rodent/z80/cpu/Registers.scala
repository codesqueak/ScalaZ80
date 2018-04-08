package com.rodent.z80.cpu

import com.rodent.z80.CPUZ.RegNames
import com.rodent.z80.CPUZ.RegNames.RegName
import com.rodent.z80.func.Utils

case class Registers(regFile1: BaseRegisters = BaseRegisters(),
                     regFile2: BaseRegisters = BaseRegisters(),
                     index: IndexRegisters = IndexRegisters(),
                     control: ControlRegisters = ControlRegisters(),
                     internal: InternalRegisters = InternalRegisters()) {

  // flag register bits
  private val s = 0x80
  private val z = 0x40
  private val f5 = 0x20
  private val h = 0x10
  private val f3 = 0x08
  private val pv = 0x04
  private val n = 0x02
  private val c = 0x01
  private val ns = s ^ 0xFF
  private val nz = z ^ 0xFF
  private val nf5 = f5 ^ 0xFF
  private val nh = h ^ 0xFF
  private val nf3 = f3 ^ 0xFF
  private val npv = pv ^ 0xFF
  private val nn = n ^ 0xFF
  private val nc = c ^ 0xFF

  // general getters
  def getPC: Int = control.pc

  def getSP: Int = control.sp

  def getInst: Int = internal.inst

  def getA: Int = regFile1.a

  // decoded instruction bit fields
  def getDecodeInst: Int = internal.x

  def src: Int = internal.y

  def dst: Int = internal.z

  def p: Int = internal.p

  def q: Int = internal.q

  def dd: Boolean = internal.dd

  def fd: Boolean = internal.fd

  def single: Boolean = internal.single

  def halt: Boolean = internal.halt

  def setHalt: InternalRegisters = {
    internal.copy(halt = true)
  }

  // reg get
  def getReg(reg: RegName): Int = {
    reg match {
      case RegNames.A => regFile1.a
      case RegNames.F => regFile1.f
      case RegNames.B => regFile1.b
      case RegNames.C => regFile1.c
      case RegNames.D => regFile1.d
      case RegNames.E => regFile1.e
      case RegNames.H => regFile1.h
      case RegNames.L => regFile1.l
      case RegNames.DATA8 => regFile1.data8.get
      case RegNames.IX => index.ix
      case RegNames.IY => index.iy
      case RegNames.PC => control.pc
      case RegNames.SP => control.sp
      case RegNames.R => control.r
      case RegNames.DATA16 => regFile1.data16.get
      case RegNames.WZ => regFile1.wz.get
      case RegNames.INST => internal.inst
      case RegNames.IXIYH if dd => (index.ix & 0xFF00) >> 8
      case RegNames.IXIYL if dd => index.ix & 0x00FF
      case RegNames.IXIYH if fd => (index.iy & 0xFF00) >> 8
      case RegNames.IXIYL if fd => index.iy & 0x00FF
    }
  }

  def getReg16(reg: RegName): Int = {
    reg match {
      case RegNames.A => (regFile1.a << 8) + regFile1.f
      case RegNames.B => (regFile1.b << 8) + regFile1.c
      case RegNames.D => (regFile1.d << 8) + regFile1.e
      case RegNames.H => (regFile1.h << 8) + regFile1.l
      case RegNames.IX => index.ix
      case RegNames.IY => index.iy
      case RegNames.PC => control.pc
      case RegNames.SP => control.sp
      case RegNames.DATA16 => regFile1.data16.get
      case RegNames.WZ => regFile1.wz.get
    }
  }

  def getReg16Index(reg: RegName): Int = {
    reg match {
      case RegNames.A => (regFile1.a << 8) + regFile1.f
      case RegNames.B => (regFile1.b << 8) + regFile1.c
      case RegNames.D => (regFile1.d << 8) + regFile1.e
      case RegNames.H if internal.dd => index.ix
      case RegNames.H if internal.fd => index.iy
      case RegNames.H => (regFile1.h << 8) + regFile1.l
      case RegNames.IX => index.ix
      case RegNames.IY => index.iy
      case RegNames.PC => control.pc
      case RegNames.SP => control.sp
      case RegNames.DATA16 => regFile1.data16.get
      case RegNames.WZ => regFile1.wz.get
    }
  }

  def getAltReg16(reg: RegName): Int = {
    reg match {
      case RegNames.A => (regFile2.a << 8) + regFile2.f
      case RegNames.B => (regFile2.b << 8) + regFile2.c
      case RegNames.D => (regFile2.d << 8) + regFile2.e
      case RegNames.H => (regFile2.h << 8) + regFile2.l
      case RegNames.IX => index.ix
      case RegNames.IY => index.iy
      case RegNames.PC => control.pc
      case RegNames.SP => control.sp
      case RegNames.DATA16 => regFile2.data16.get
      case RegNames.WZ => regFile2.wz.get
    }
  }

  def setBaseReg(reg: RegName, v: Int): BaseRegisters = {
    reg match {
      case RegNames.A => regFile1.copy(a = v)
      case RegNames.F => regFile1.copy(f = v)
      case RegNames.B => regFile1.copy(b = v)
      case RegNames.C => regFile1.copy(c = v)
      case RegNames.D => regFile1.copy(d = v)
      case RegNames.E => regFile1.copy(e = v)
      case RegNames.H => regFile1.copy(h = v)
      case RegNames.L => regFile1.copy(l = v)
      case RegNames.DATA8 => regFile1.copy(data8 = Option(v))
      case RegNames.DATA16 => regFile1.copy(data16 = Option(v))
      case RegNames.WZ => regFile1.copy(wz = Option(v))
    }
  }

  def setIndexReg(reg: RegName, v: Int): IndexRegisters = {
    reg match {
      case RegNames.IX => index.copy(ix = v)
      case RegNames.IY => index.copy(iy = v)
    }
  }

  def setPC(v: Int): ControlRegisters = {
    control.copy(pc = v)
  }

  def setSP(v: Int): ControlRegisters = {
    control.copy(sp = v)
  }

  def setControlReg(reg: RegName, v: Int): ControlRegisters = {
    reg match {
      case RegNames.PC => control.copy(pc = v)
      case RegNames.SP => control.copy(sp = v)
      case RegNames.R => control.copy(r = v)
    }
  }

  def setInternalReg(reg: RegName, v: Int): InternalRegisters = {
    reg match {
      case RegNames.X => internal.copy(x = v)
      case RegNames.Y => internal.copy(y = v)
      case RegNames.Z => internal.copy(z = v)
      case RegNames.P => internal.copy(p = v)
      case RegNames.Q => internal.copy(q = v)
    }
  }

  def setBaseReg16(reg: RegName, v: Int): BaseRegisters = {
    val msb = (v & 0xFF00) >> 8
    val lsb = v & 0x00FF
    reg match {
      case RegNames.A => regFile1.copy(a = msb, f = lsb)
      case RegNames.B => regFile1.copy(b = msb, c = lsb)
      case RegNames.D => regFile1.copy(d = msb, e = lsb)
      case RegNames.H => regFile1.copy(h = msb, l = lsb)
      case RegNames.DATA16 => regFile1.copy(data16 = Option(v))
      case RegNames.WZ => regFile1.copy(wz = Option(v))
    }
  }

  def setAltBaseReg16(reg: RegName, v: Int): BaseRegisters = {
    val msb = (v & 0xFF00) >> 8
    val lsb = v & 0x00FF
    reg match {
      case RegNames.A => regFile2.copy(a = msb, f = lsb)
      case RegNames.B => regFile2.copy(b = msb, c = lsb)
      case RegNames.D => regFile2.copy(d = msb, e = lsb)
      case RegNames.H => regFile2.copy(h = msb, l = lsb)
    }
  }

  // get flag settings
  def isS: Boolean = 0 != (regFile1.f & s)

  def isNS: Boolean = !isS

  def isZ: Boolean = 0 != (regFile1.f & z)

  def isNZ: Boolean = !isZ

  def isF5: Boolean = 0 != (regFile1.f & f5)

  def isNF5: Boolean = !isF5

  def isH: Boolean = 0 != (regFile1.f & h)

  def isNH: Boolean = !isH

  def isF3: Boolean = 0 != (regFile1.f & f3)

  def isNF3: Boolean = !isF3

  def isPV: Boolean = 0 != (regFile1.f & pv)

  def isNPV: Boolean = !isPV

  def isN: Boolean = 0 != (regFile1.f & n)

  def isNN: Boolean = !isN

  def isC: Boolean = 0 != (regFile1.f & c)

  def isNC: Boolean = !isC

  // set 8 bit reg  and associated flags
  def setFlags(sf: Option[Boolean] = None,
               zf: Option[Boolean] = None,
               f5f: Option[Boolean] = None,
               hf: Option[Boolean] = None,
               f3f: Option[Boolean] = None,
               pvf: Option[Boolean] = None,
               nf: Option[Boolean] = None,
               cf: Option[Boolean] = None
              ): BaseRegisters = {
    val flags = fixFlags(sf, zf, f5f, hf, f3f, pvf, nf, cf)
    regFile1.copy(f = flags)
  }

  // set a register and associated flags
  def setResult8(reg: RegName,
                 v: Int,
                 sf: Option[Boolean] = None,
                 zf: Option[Boolean] = None,
                 f5f: Option[Boolean] = None,
                 hf: Option[Boolean] = None,
                 f3f: Option[Boolean] = None,
                 pvf: Option[Boolean] = None,
                 nf: Option[Boolean] = None,
                 cf: Option[Boolean] = None
                ): BaseRegisters = {
    val flags = fixFlags(sf, zf, f5f, hf, f3f, pvf, nf, cf)
    setBaseReg(reg, v).copy(f = flags)
  }


  // set a register and associated flags
  def setResult8(reg: RegName,
                 v: Int
                ): BaseRegisters = {
    setBaseReg(reg, v)
  }

  // set a register and associated flags
  def setResult8(reg: RegName,
                 v: Int,
                 flags: Int
                ): BaseRegisters = {
    setBaseReg(reg, v).copy(f = flags)
  }

  // set A and associated flags
  def setResultA(v: Int,
                 sf: Option[Boolean] = None,
                 zf: Option[Boolean] = None,
                 f5f: Option[Boolean] = None,
                 hf: Option[Boolean] = None,
                 f3f: Option[Boolean] = None,
                 pvf: Option[Boolean] = None,
                 nf: Option[Boolean] = None,
                 cf: Option[Boolean] = None
                ): BaseRegisters = {
    val flags = fixFlags(sf, zf, f5f, hf, f3f, pvf, nf, cf)
    regFile1.copy(a = v, f = flags)
  }

  // set HL and associated flags
  def setResultHL(v: Int,
                  sf: Option[Boolean] = None,
                  zf: Option[Boolean] = None,
                  f5f: Option[Boolean] = None,
                  hf: Option[Boolean] = None,
                  f3f: Option[Boolean] = None,
                  pvf: Option[Boolean] = None,
                  nf: Option[Boolean] = None,
                  cf: Option[Boolean] = None
                 ): BaseRegisters = {
    val flags = fixFlags(sf, zf, f5f, hf, f3f, pvf, nf, cf)
    regFile1.copy(h = (v & 0xFF00) >> 8, l = v & 0x00FF, f = flags)
  }

  // set IX
  def setResultIX(v: Int): IndexRegisters = {
    index.copy(ix = v)
  }

  // set IY
  def setResultIY(v: Int): IndexRegisters = {
    index.copy(iy = v)
  }

  private def fixFlags(sf: Option[Boolean], zf: Option[Boolean], f5f: Option[Boolean], hf: Option[Boolean], f3f: Option[Boolean], pvf: Option[Boolean], nf: Option[Boolean], cf: Option[Boolean]): Int = {
    var flags = regFile1.f
    flags = sf.map(if (_) flags | s else flags & ns).getOrElse(flags)
    flags = zf.map(if (_) flags | z else flags & nz).getOrElse(flags)
    flags = f5f.map(if (_) flags | f5 else flags & nf5).getOrElse(flags)
    flags = hf.map(if (_) flags | h else flags & nh).getOrElse(flags)
    flags = f3f.map(if (_) flags | f3 else flags & nf3).getOrElse(flags)
    flags = pvf.map(if (_) flags | pv else flags & npv).getOrElse(flags)
    flags = nf.map(if (_) flags | n else flags & nn).getOrElse(flags)
    flags = cf.map(if (_) flags | c else flags & nc).getOrElse(flags)
    flags
  }

  // debug register dump
  def dump(): Unit = {
    for (c <- RegNames.values) {
      val v = getReg(c)
      println(c + " " + Utils.toHex16(v))
    }
  }

}
