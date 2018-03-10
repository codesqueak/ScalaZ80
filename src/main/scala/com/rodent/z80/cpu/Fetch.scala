package com.rodent.z80.cpu

import com.rodent.z80.CPUZ.RegNames
import com.rodent.z80.func.Utils
import com.rodent.z80.io.Memory

// Execute an instruction fetch and increment program counter

trait Fetch {

  val memory: Memory

  def fetch(registers: Registers): Registers = {
    var pc = registers.getPC
    val ir = registers.internalRegisters.copy(inst = memory.getMemory(pc))
    println("Execute @" + Utils.toHex16(registers.getPC) + " : " + Utils.toHex8(ir.inst) + " sp:" + Utils.toHex16(registers.getSP) + regs(registers))
    pc = (pc + 1) & 0xFFFF
    val cr = registers.controlRegisters.copy(pc = pc)
    registers.copy(internalRegisters = ir, controlRegisters = cr)
  }

  def regs(registers: Registers): String = {
    "  AF:" + Utils.toHex16(registers.getReg16(RegNames.A)) +
      "  BC:" + Utils.toHex16(registers.getReg16(RegNames.B)) +
      "  DE:" + Utils.toHex16(registers.getReg16(RegNames.D)) +
      "  HL:" + Utils.toHex16(registers.getReg16(RegNames.H)) +
      "  IX:" + Utils.toHex16(registers.getReg16(RegNames.IX)) +
      "  IY:" + Utils.toHex16(registers.getReg16(RegNames.IY))
  }
}
