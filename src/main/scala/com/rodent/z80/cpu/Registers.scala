package com.rodent.z80.cpu

import com.rodent.z80.RegNames
import com.rodent.z80.RegNames._
import com.rodent.z80.func._

trait Registers {

  private var pc: Int = 0;
  private var sp: Int = 0;
  // user registers
  private var (a, f, b, c, d, e, h, l, ix, iy, r) = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  private var (ax, fx, bx, cx, dx, ex, hx, lx) = (0, 0, 0, 0, 0, 0, 0, 0 )
  // internal registers
  private var (m8, m16) = (0, 0)
  private var decode = (0, 0, 0, 0, 0)
  // flags
  private var halt: Boolean = false

  // general getters
  def getPC: Int = pc

  def getInst: Int = m8

  def getDecodeInst: Int = decode._1

  def src: Int = decode._2

  def dst: Int = decode._3
  def p: Int = decode._4
  def q: Int = decode._5

  // general setters
  def setPC(addr: Int): Unit = pc = addr

  def setInst(v: Int): Unit = m8 = v

  def setDecode(v: (Int, Int, Int, Int, Int)): Unit = decode = v

  // PC stuff
  def incPC: Int = {
    pc = (pc + 1) & 0xFFFF
    pc
  }

  // reg get / set
  def getReg(reg: RegName): Int = {
    reg match {
      case RegNames.A => a
      case RegNames.F => f
      case RegNames.B => b
      case RegNames.C => c
      case RegNames.D => d
      case RegNames.E => e
      case RegNames.H => h
      case RegNames.L => l
      case RegNames.IX => ix
      case RegNames.IY => iy
      case RegNames.PC => pc
      case RegNames.SP => sp
      case RegNames.R => r
      case RegNames.M8 => m8
      case RegNames.M16 => m16
    }
  }

  def getReg16(reg: RegName): Int = {
    reg match {
      case RegNames.A => (a << 8) + f
      case RegNames.B => (b << 8) + c
      case RegNames.D => (d << 8) + e
      case RegNames.H => (h << 8) + l
      case RegNames.IX => ix
      case RegNames.IY => iy
      case RegNames.PC => pc
      case RegNames.SP => sp
      case RegNames.M16 => m16
    }
  }

  def setReg(reg: RegName, v: Int): Unit = {
    reg match {
      case RegNames.A => a = v
      case RegNames.F => f = v
      case RegNames.B => b = v
      case RegNames.C => c = v
      case RegNames.D => d = v
      case RegNames.E => e = v
      case RegNames.H => h = v
      case RegNames.L => l = v
      case RegNames.IX => ix = v
      case RegNames.IY => iy = v
      case RegNames.PC => pc = v
      case RegNames.SP => sp = v
      case RegNames.R => r = v
      case RegNames.M8 => m8 = v
      case RegNames.M16 => m16 = v
    }
  }

  def setReg16(reg: RegName, v: Int): Unit = {
    reg match {
      case RegNames.A => {
        a = (v & 0xFF00) >> 8
        f = v & 0x00FF
      }
      case RegNames.B => {
        b = (v & 0xFF00) >> 8
        c = v & 0x00FF
      }
      case RegNames.D => {
        d = (v & 0xFF00) >> 8
        e = v & 0x00FF
      }
      case RegNames.H => {
        h = (v & 0xFF00) >> 8
        l = v & 0x00FF
      }
      case RegNames.IX => ix = v
      case RegNames.IY => iy = v
      case RegNames.PC => pc = v
      case RegNames.SP => sp = v
      case RegNames.M16 => m16 = v
    }
  }

  def dump(): Unit = {
    for (c <- RegNames.values) println(c + " " + util.toHex16(getReg(c)))
  }

}
