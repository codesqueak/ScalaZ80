package com.rodent.z80.cpu

import com.rodent.z80.func.Utils
import com.rodent.z80.io.Memory

// Process additional memory writes where required

trait OptionalWriter {

  val memory: Memory

  def write(r: Registers): Registers = {
    if (r.regFile1.wz.isDefined) {
      if (r.regFile1.data8.isDefined) {
        memory.setMemory(r.regFile1.wz.get, r.regFile1.data8.get)
        //    println("writing " + Utils.toHex8(r.regFile1.data8.get) + " to " + Utils.toHex16(r.regFile1.wz))
        r.copy(regFile1 = r.regFile1.copy(data8 = None, data16 = None, wz = None))
      }
      else if (r.regFile1.data16.isDefined) {
        memory.setMemory16(r.regFile1.wz.get, r.regFile1.data16.get)
        //     println("writing " + Utils.toHex16(r.regFile1.data16.get) + " to " + Utils.toHex16(r.regFile1.wz))
        r.copy(regFile1 = r.regFile1.copy(data8 = None, data16 = None, wz = None))
      }
      else
        throw new UndefOpcode("Addr: " + Utils.toHex16(r.getPC) + " wz set but no data available")
    }
    else
      r
  }
}

