package com.rodent.z80.cpu

import com.rodent.z80.CPUZ.RegNames
import com.rodent.z80.func.Utils
import com.rodent.z80.io.Memory

// Execute an instruction fetch and increment program counter

trait Fetch {

  val memory: Memory
  var count = 0

  def fetch(registers: Registers): Registers = {
    var pc = registers.getPC
    val ir = registers.internalRegisters.copy(inst = memory.getMemory(pc))
    if (ir.single) {
      //    println(count+" >> Execute @" + Utils.toHex16(registers.getPC) + " : " + Utils.toHex8(ir.inst) + " SP:" + Utils.toHex16(registers.getSP) + regs(registers))
      count += 1
      //  if (count > 5498) System.exit(0)
    }
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
