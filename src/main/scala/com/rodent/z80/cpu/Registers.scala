package com.rodent.z80.cpu

import com.rodent.z80.RegNames
import com.rodent.z80.RegNames._
import com.rodent.z80.func._

case class Registers(regFile1: BaseRegisters = BaseRegisters(),
                     regFile2: BaseRegisters = BaseRegisters(),
                     indexRegisters: IndexRegisters = IndexRegisters(),
                     controlRegisters: ControlRegisters = ControlRegisters(),
                     internalRegisters: InternalRegisters = InternalRegisters()) {

  // flags

  // general getters
  def getPC: Int = controlRegisters.pc

  def getInst: Int = internalRegisters.inst

  def getDecodeInst: Int = internalRegisters.x

  def src: Int = internalRegisters.y

  def dst: Int = internalRegisters.z

  def p: Int = internalRegisters.p

  def q: Int = internalRegisters.q

  // reg get / set
  private def getSafeReg(reg: RegName): Option[Int] = {
    reg match {
      case RegNames.A => Some(regFile1.a)
      case RegNames.F => Some(regFile1.f)
      case RegNames.B => Some(regFile1.b)
      case RegNames.C => Some(regFile1.c)
      case RegNames.D => Some(regFile1.d)
      case RegNames.E => Some(regFile1.e)
      case RegNames.H => Some(regFile1.h)
      case RegNames.L => Some(regFile1.l)
      case RegNames.M8 => Some(regFile1.m8)
      case RegNames.IX => Some(indexRegisters.ix)
      case RegNames.IY => Some(indexRegisters.iy)
      case RegNames.PC => Some(controlRegisters.pc)
      case RegNames.SP => Some(controlRegisters.sp)
      case RegNames.R => Some(controlRegisters.r)
      case RegNames.M16 => Some(internalRegisters.m16)
      case RegNames.INST => Some(internalRegisters.inst)
      case RegNames.ADDR => Some(internalRegisters.addr)
      case _ => None
    }
  }

  // reg get / set
  def getReg(reg: RegName): Int = {
    getSafeReg(reg).get
  }

  def getReg16(reg: RegName): Int = {
    reg match {
      case RegNames.A => (regFile1.a << 8) + regFile1.f
      case RegNames.B => (regFile1.b << 8) + regFile1.c
      case RegNames.D => (regFile1.d << 8) + regFile1.e
      case RegNames.H => (regFile1.h << 8) + regFile1.l
      case RegNames.IX => indexRegisters.ix
      case RegNames.IY => indexRegisters.iy
      case RegNames.PC => controlRegisters.pc
      case RegNames.SP => controlRegisters.sp
      case RegNames.M16 => internalRegisters.m16
      case RegNames.ADDR => internalRegisters.addr
    }
  }

  def setBaseReg(reg: RegName, v: Int): BaseRegisters = {
    reg match {
      case RegNames.A => regFile1.copy(a = v)
      case RegNames.F => regFile1.copy(f = v)
      case RegNames.B => regFile1.copy(b = v)
      case RegNames.C => regFile1.copy(c = v)
      case RegNames.D => regFile1.copy(e = v)
      case RegNames.E => regFile1.copy(e = v)
      case RegNames.H => regFile1.copy(h = v)
      case RegNames.L => regFile1.copy(l = v)
      case RegNames.M8 => regFile1.copy(m8 = v)
    }
  }

  def setIndexReg(reg: RegName, v: Int): IndexRegisters = {
    reg match {
      case RegNames.IX => indexRegisters.copy(ix = v)
      case RegNames.IY => indexRegisters.copy(iy = v)
    }
  }

  def setControlReg(reg: RegName, v: Int): ControlRegisters = {
    reg match {
      case RegNames.PC => controlRegisters.copy(pc = v)
      case RegNames.SP => controlRegisters.copy(sp = v)
      case RegNames.R => controlRegisters.copy(r = v)
    }
  }

  def setInternalReg(reg: RegName, v: Int): InternalRegisters = {
    reg match {
      case RegNames.M16 => internalRegisters.copy(m16 = v)
      case RegNames.ADDR => internalRegisters.copy(addr = v)
      case RegNames.X => internalRegisters.copy(x = v)
      case RegNames.Y => internalRegisters.copy(y = v)
      case RegNames.Z => internalRegisters.copy(z = v)
      case RegNames.P => internalRegisters.copy(p = v)
      case RegNames.Q => internalRegisters.copy(q = v)
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
    }
  }

  def dump(): Unit = {
    for (c <- RegNames.values) {
      val v = getSafeReg(c)
      v.foreach(v => println(c + " " + utils.toHex16(v)))
    }
  }

}
