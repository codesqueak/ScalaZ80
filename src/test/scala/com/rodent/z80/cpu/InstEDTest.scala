package com.rodent.z80.cpu

import com.rodent.z80.CPUZ.RegNames
import com.rodent.z80.io.{Memory, Ports}
import org.scalatest.{FlatSpec, Matchers}

class InstEDTest extends FlatSpec with Matchers {


  "Block move" should "copy correctly" in {
    var memory = new Memory()
    var loc = 0xC000
    loc = memory.setMemory(loc, Array(0x21, 0x00, 0x10)) // ld hl,1000
    loc = memory.setMemory(loc, Array(0x11, 0x00, 0x20)) // ld de,2000
    loc = memory.setMemory(loc, Array(0x03, 0x03)) // inc bc
    loc = memory.setMemory(loc, Array(0xED, 0xB0)) // ldir
    loc = memory.setMemory(loc, Array(0x03, 0x03)) // inc bc
    loc = memory.setMemory(loc, Array(0xED, 0xB8)) // lddr
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    memory.setMemory(0x1000, Array(0x12, 0x34, 0x56))

    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.B) should be(0x0000)
    cpu.registers.getReg16(RegNames.D) should be(0x2000)
    cpu.registers.getReg16(RegNames.H) should be(0x1000)
    //
    memory.getMemory(0x2000) should be(0x12)
    memory.getMemory(0x2001) should be(0x34)
    memory.getMemory(0x2002) should be(0x56)
  }


}
