package com.rodent.z80.cpu

import com.rodent.z80.CPUZ.RegNames
import com.rodent.z80.io.{Memory, Ports}
import org.scalatest.{FlatSpec, Matchers}

class InstDDFDCBTest extends FlatSpec with Matchers {

  "bit (ix+dd)" should "execute correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0xdd, 0x21, 0x02, 0x10)) // ld ix,1002
    loc = memory.setMemory(loc, Array(0xdd, 0xcb, 0xff, 0x46)) // bit 0, (ix+00)
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    memory.setMemory(0x1000, Array(0x01, 0x02, 0x03, 0x04, 0x05))
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.IX) should be(0x1002)
    cpu.registers.isZ should be(true)
  }


  "set (ix+dd)" should "execute correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0xdd, 0x21, 0x00, 0x10)) // ld ix,1002
    loc = memory.setMemory(loc, Array(0xdd, 0xcb, 0x01, 0xc6)) // bit 0, (ix+00)
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    memory.setMemory(0x1000, Array(0x01, 0x02, 0x03, 0x04, 0x05))
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.IX) should be(0x1000)
    //
    memory.getMemory(0x1000) should be(0x01)
    memory.getMemory(0x1001) should be(0x03)
    memory.getMemory(0x1002) should be(0x03)
  }

  "reset (ix+dd)" should "execute correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0xdd, 0x21, 0x00, 0x10)) // ld ix,1002
    loc = memory.setMemory(loc, Array(0xdd, 0xcb, 0x02, 0x8e)) // bit 0, (ix+00)
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    memory.setMemory(0x1000, Array(0x01, 0x02, 0x03, 0x04, 0x05))
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.IX) should be(0x1000)
    //
    memory.getMemory(0x1000) should be(0x01)
    memory.getMemory(0x1001) should be(0x02)
    memory.getMemory(0x1002) should be(0x01)
    memory.getMemory(0x1003) should be(0x04)
  }

}
