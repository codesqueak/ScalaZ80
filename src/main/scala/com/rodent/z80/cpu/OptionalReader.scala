package com.rodent.z80.cpu

import com.rodent.z80.io.Memory

trait OptionalReader {
  val memory: Memory

  def read(registers: Registers): Registers = {
    registers
  }

}
