package com.rodent.z80.cpu

import com.rodent.z80.io.Memory

trait OptionalWriter {
  def write(registers: Registers, memory: Memory): Registers = {
    registers
  }

}
