package com.rodent.z80.cpu

import com.rodent.z80.CPUZ.RegNames
import com.rodent.z80.io.{Memory, Ports}
import org.scalatest.{FlatSpec, Matchers}

class InstIXIYTest extends FlatSpec with Matchers {

  "JP (IX)" should "jump correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0xdd, 0x21, 0x00, 0x10)) // ld ix, 1000
    loc = memory.setMemory(loc, Array(0xdd, 0xe9)) // jp (ix)
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.IX) should be(0x1000)
    cpu.registers.getReg16(RegNames.PC) should be(0x1001)
  }

  "LD SP,IX" should "load stack pointer correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0xdd, 0x21, 0x34, 0x56)) // ld ix, 5634
    loc = memory.setMemory(loc, Array(0xdd, 0xf9)) // jp (ix)
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.IX) should be(0x5634)
    cpu.registers.getReg16(RegNames.SP) should be(0x5634)
  }

  "EX (SP),IX" should "lolad stack pointer correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x31, 0x00, 0x10)) // ld sp,1000
    loc = memory.setMemory(loc, Array(0xdd, 0x21, 0x34, 0x56)) // ld ix, 5634
    loc = memory.setMemory(loc, Array(0x21, 0x78, 0x9a)) // ld hl, 9a78
    loc = memory.setMemory(loc, Array(0xe5)) // push hl
    loc = memory.setMemory(loc, Array(0xdd, 0xe3)) // ex (sp),ix
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    memory.getMemory(0x0FFF) should be(0x9a)
    memory.getMemory(0x0FFE) should be(0x78)
    cpu.registers.getReg16(RegNames.IX) should be(0x9a78)
    cpu.registers.getReg16(RegNames.H) should be(0x9a78)
    cpu.registers.getReg16(RegNames.SP) should be(0x0ffe)
  }

}
