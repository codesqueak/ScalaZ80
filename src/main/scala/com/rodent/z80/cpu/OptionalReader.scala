package com.rodent.z80.cpu

import com.rodent.z80.io.Memory

trait OptionalReader {
  def read(registers: Registers, memory: Memory): Registers = {
    registers
  }

}
