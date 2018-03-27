package com.rodent.z80.cpu

import com.rodent.z80.CPUZ.RegNames
import com.rodent.z80.io.{Memory, Ports}
import org.scalatest.{FlatSpec, Matchers}

class OneByteInstTest extends FlatSpec with Matchers {

  "inc/dec a" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x3e, 0x7f)) // ld a,00
    loc = memory.setMemory(loc, Array(0x3c)) // inc a
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg(RegNames.A) should be(0x80)
    cpu.registers.isS should be(true)
    cpu.registers.isZ should be(false)
    cpu.registers.isF5 should be(false)
    cpu.registers.isH should be(true)
    cpu.registers.isF3 should be(false)
    cpu.registers.isPV should be(true)
    cpu.registers.isN should be(false)
    cpu.registers.isC should be(false)
  }

  "inc in memory" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x21, 0x00, 0x20)) // ld hl,2000
    loc = memory.setMemory(loc, Array(0x34)) // inc (hl)
    loc = memory.setMemory(loc, Array(0x34)) // inc (hl)
    loc = memory.setMemory(loc, Array(0x34)) // inc (hl)
    loc = memory.setMemory(loc, Array(0x34)) // inc (hl)
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    memory.setMemory(0x2000, Array(0x00)) // test data
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.H) should be(0x2000)
    //
    memory.getMemory(0x2000) should be(0x04)
    //
    cpu.registers.isS should be(false)
    cpu.registers.isZ should be(false)
    cpu.registers.isF5 should be(false)
    cpu.registers.isH should be(false)
    cpu.registers.isF3 should be(false)
    cpu.registers.isPV should be(false)
    cpu.registers.isN should be(false)
    cpu.registers.isC should be(false)

  }

  "dec in memory" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x21, 0x00, 0x20)) // ld hl,2000
    loc = memory.setMemory(loc, Array(0x35)) // inc (hl)
    loc = memory.setMemory(loc, Array(0x35)) // inc (hl)
    loc = memory.setMemory(loc, Array(0x35)) // inc (hl)
    loc = memory.setMemory(loc, Array(0x35)) // inc (hl)
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    memory.setMemory(0x2000, Array(0x03)) // test data
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.registers = cpu.registers.copy(regFile1 = cpu.registers.setFlags(cf = Option(true)))
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.H) should be(0x2000)
    //
    memory.getMemory(0x2000) should be(0xFF)
    //
    cpu.registers.isS should be(true)
    cpu.registers.isZ should be(false)
    cpu.registers.isF5 should be(true)
    cpu.registers.isH should be(true)
    cpu.registers.isF3 should be(true)
    cpu.registers.isPV should be(false)
    cpu.registers.isN should be(true)
    cpu.registers.isC should be(true)
  }

}
