package com.rodent.z80.cpu

case class InternalRegisters(inst: Int = 0, x: Int = 0, y: Int = 0, z: Int = 0, p: Int = 0, q: Int = 0,
                             single: Boolean = true, cb: Boolean = false, dd: Boolean = false, ed: Boolean = false, fd: Boolean = false,
                             halt: Boolean = false) {}
