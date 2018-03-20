package com.rodent.z80.cpu

import com.rodent.z80.CPUZ.RegNames
import com.rodent.z80.io.{Memory, Ports}
import org.scalatest.{FlatSpec, Matchers}

class InstEDTest extends FlatSpec with Matchers {


  "Block move" should "copy correctly" in {
    var memory = new Memory()
    var loc = 0xC000
    memory.setMemory(loc, 0x21) // ld hl,1000
    loc += 1
    memory.setMemory(loc, 0x00)
    loc += 1
    memory.setMemory(loc, 0x10)
    loc += 1
    memory.setMemory(loc, 0x11) // ld de,2000
    loc += 1
    memory.setMemory(loc, 0x00)
    loc += 1
    memory.setMemory(loc, 0x20)
    loc += 1
    memory.setMemory(loc, 0x03) // inc bc
    loc += 1
    memory.setMemory(loc, 0x03)
    loc += 1
    memory.setMemory(loc, 0xED) // ldir
    loc += 1
    memory.setMemory(loc, 0xB0) //
    loc += 1
    memory.setMemory(loc, 0x03) // inc BC
    loc += 1
    memory.setMemory(loc, 0x03)
    loc += 1
    memory.setMemory(loc, 0xED) // lddr
    loc += 1
    memory.setMemory(loc, 0xB8)
    loc += 1
    memory.setMemory(loc, 0x76) // halt
    //
    memory.setMemory(0x1000, 0x12)
    memory.setMemory(0x1001, 0x34)
    memory.setMemory(0x1002, 0x56)
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
