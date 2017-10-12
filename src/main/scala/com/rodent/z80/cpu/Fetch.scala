package com.rodent.z80.cpu

import com.rodent.z80.io.Memory

trait Fetch {
  def fetch(registers: Registers, memory: Memory): Registers = {
    registers
  }

}
