package com.rodent.z80.cpu

import com.rodent.z80.io.Memory

trait OptionalWriter {
  val memory: Memory

  def write(registers: Registers): Registers = {
    registers
  }

}
