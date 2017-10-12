package com.rodent.z80.cpu

import com.rodent.z80.RegNames
import com.rodent.z80.RegNames.RegName
import com.rodent.z80.func.utils
import com.rodent.z80.io._

// Z80 Processor core

class Processor(memory: Memory, ports: Ports, registers: Registers) extends Fetch {

  val reg8Bit = Map(0 -> RegNames.B, 1 -> RegNames.C, 2 -> RegNames.D, 3 -> RegNames.E, 4 -> RegNames.H, 5 -> RegNames.L, 6 -> RegNames.M8, 7 -> RegNames.A)
  val reg16Bit = Map(0 -> RegNames.B, 1 -> RegNames.D, 2 -> RegNames.H, 3 -> RegNames.SP)

  var pendingWrites: List[(Int, Int)] = Nil

  def run(): Unit = {
    var continue: Boolean = true
    // Pipeline fetch -> decode -> read(opt) -> execute -> write(opt)
    while (continue) {
      if (fetch) {
        decode()
        execute()
        write()
      }
      else continue = false
    }
  }

  // basic pipeling - fetch - decode - execute - write
  def fetch: Boolean = {
    val inst = memFetch
    println("Execute @" + utils.toHex16(registers.controlRegisters.pc) + " : " + utils.toHex8(registers.internalRegisters.inst))
    inst != 0x76
  }

  // Basic decode step - inst / source / destination / p / q
  def decode(): InternalRegisters = {
    var m = registers.getReg(RegNames.M8)
    val i = (m & 0xC0) >> 6
    val src = (m & 0x38) >> 3
    val dst = m & 0x07
    val p = src >> 1
    val q = src & 0x01
    InternalRegisters(x = i, y = src, z = dst, p = p, q = q)
  }

  // ALU

  // Execute an instruction
  def execute(): Unit = {
    registers.internalRegisters.x match {
      case 0 => general0(registers.internalRegisters)
      case 1 => load8(registers.internalRegisters)
      case 2 => println("todo 1")
      case 3 => println("todo 2")
    }
  }

  // Write results back to memory & IO
  def write(): Unit = {}

  // Memory fetch cycle
  def memFetch: Int = {
    val pc = registers.controlRegisters.pc
    val m = memory.getMemory(pc)
    setInst(m)
    incPC
    m
  }

  // standard 8 bit ld src,dst instrucitons
  def load8(ir: InternalRegisters): Unit = {
    if (ir.x == 6) loadFromHL()
    val srcReg = reg8Bit(ir.x)
    val dstReg = reg8Bit(ir.y)
    setReg(dstReg, registers.getReg(srcReg))
    if (ir.x == 6) loadToHL(RegNames.M8)
  }

  // instruction prefix 0
  def general0(ir: InternalRegisters): Unit = {
    ir.z match {
      case 0 =>
      case 1 => ldadd16(ir.p, ir.q)
      case 2 =>
      case 3 =>
      case 4 => inc8(ir.y)
      case 5 => dec8(ir.y)
      case 6 =>
    }
  }

  // LD / ADD 16
  def ldadd16(p: Int, q: Int): Unit = {
    if (q == 0) {
      // LD rr,(mm)
      val lsb = memFetch
      val v = memFetch << 8 + lsb
      setReg16(reg16Bit(p), v)
    } else {
      // ADD HL,rr
      val v = registers.getReg16(RegNames.H) + registers.getReg16(reg16Bit(p))
    }
  }

  // 8 bit inc
  def inc8(src: Int): Unit = {
    if (src == 6) loadFromHL()
    val reg = reg8Bit(src)
    var v = registers.getReg(reg)
    v = (v + 1) & 0xFF
    setReg(reg, v)
    if (src == 6) loadToHL(reg)
  }

  // 8 bit dec
  def dec8(src: Int): Unit = {
    if (src == 6) loadFromHL()
    val reg = reg8Bit(src)
    var v = registers.getReg(reg)
    v = (v - 1) & 0xFF
    setReg(reg, v)
    if (src == 6) loadToHL(reg)
  }

  // Utility operations

  // Load from memory (HL)
  def loadFromHL(): Unit = {
    val addr = registers.getReg16(RegNames.H)
    registers.setReg(RegNames.M8, memory.getMemory(addr))
  }

  // Save to memory (HL)
  def loadToHL(dstReg: RegName): Unit = {
    val addr = registers.getReg16(RegNames.H)
    memory.setMemory(addr, registers.getReg(dstReg))
  }
}