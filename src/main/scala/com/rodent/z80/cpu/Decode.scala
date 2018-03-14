package com.rodent.z80.cpu

trait Decode {

  // One byte instruction decoder

  def decode(registers: Registers): Registers = {
    if (registers.internalRegisters.ddcb_load || registers.internalRegisters.fdcb_load)
      registers
    else {
      var m = registers.getInst
      val i = (m & 0xC0) >> 6
      val src = (m & 0x38) >> 3
      val dst = m & 0x07
      val p = src >> 1
      val q = src & 0x01
      val ir = registers.internalRegisters.copy(x = i, y = src, z = dst, p = p, q = q)
      registers.copy(internalRegisters = ir)
    }
  }

}
