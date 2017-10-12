package com.rodent.z80.cpu

import com.rodent.z80.func.utils
import com.rodent.z80.io.Memory

// Execute an instruction fetch and increment program counter

trait Fetch {

  val memory: Memory

  def fetch(registers: Registers): Registers = {
    var pc = registers.getPC
    val ir = registers.internalRegisters.copy(inst = memory.getMemory(pc))
    println("Execute @" + utils.toHex16(registers.controlRegisters.pc) + " : " + utils.toHex8(registers.internalRegisters.inst))
    pc = (pc + 1) & 0xFFFF
    val cr = registers.controlRegisters.copy(pc = pc)
    registers.copy(internalRegisters = ir, controlRegisters = cr)
  }
}