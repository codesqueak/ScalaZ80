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


  "add in memory" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x21, 0x00, 0x20)) // ld hl,2000
    loc = memory.setMemory(loc, Array(0x3e, 0x7f)) // ld a,7f
    loc = memory.setMemory(loc, Array(0x86)) // add a,(hl)
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    memory.setMemory(0x2000, Array(0x22)) // test data
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.registers = cpu.registers.copy(regFile1 = cpu.registers.setFlags(cf = Option(true)))
    cpu.run(0xC000)
    //
    cpu.registers.getReg(RegNames.A) should be(0xa1)
    cpu.registers.getReg16(RegNames.H) should be(0x2000)
    //
    memory.getMemory(0x2000) should be(0x22)
  }

  "add in memory with carry" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x21, 0x00, 0x20)) // ld hl,2000
    loc = memory.setMemory(loc, Array(0x3e, 0x7f)) // ld a,7f
    loc = memory.setMemory(loc, Array(0x8e)) // adc a,(hl)
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    memory.setMemory(0x2000, Array(0x22)) // test data
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.registers = cpu.registers.copy(regFile1 = cpu.registers.setFlags(cf = Option(true)))
    cpu.run(0xC000)
    //
    cpu.registers.getReg(RegNames.A) should be(0xa2)
    cpu.registers.getReg16(RegNames.H) should be(0x2000)
    //
    memory.getMemory(0x2000) should be(0x22)
  }

  "sbc in memory" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x21, 0x00, 0x20)) // ld hl,2000
    loc = memory.setMemory(loc, Array(0x3e, 0x11)) // ld a,11
    loc = memory.setMemory(loc, Array(0x9e)) // sub a,(hl)
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    memory.setMemory(0x2000, Array(0x12)) // test data
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.registers = cpu.registers.copy(regFile1 = cpu.registers.setFlags(cf = Option(true)))
    cpu.run(0xC000)
    //
    cpu.registers.getReg(RegNames.A) should be(0xfe)
    cpu.registers.getReg16(RegNames.H) should be(0x2000)
    //
    memory.getMemory(0x2000) should be(0x12)
    //
  }

  "ccf" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x3e, 0x7f)) // ld a,7f
    loc = memory.setMemory(loc, Array(0x3f)) // ccf
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.registers = cpu.registers.copy(regFile1 = cpu.registers.setFlags(cf = Option(false)))
    cpu.run(0xC000)
    //
    cpu.registers.getReg(RegNames.A) should be(0x7f)
    cpu.registers.isC should be(true)
    cpu.registers.isH should be(false)
    cpu.registers.isF3 should be(true)
    cpu.registers.isF5 should be(true)
  }


  "scf" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x3e, 0x7f)) // ld a,7f
    loc = memory.setMemory(loc, Array(0x37)) // scf
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.registers = cpu.registers.copy(regFile1 = cpu.registers.setFlags(cf = Option(false)))
    cpu.run(0xC000)
    //
    cpu.registers.getReg(RegNames.A) should be(0x7f)
    cpu.registers.isC should be(true)
    cpu.registers.isH should be(false)
    cpu.registers.isF3 should be(true)
    cpu.registers.isF5 should be(true)
  }


  "cpl" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x3e, 0x7f)) // ld a,7f
    loc = memory.setMemory(loc, Array(0x2f)) // cpl
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.registers = cpu.registers.copy(regFile1 = cpu.registers.setFlags(cf = Option(false)))
    cpu.run(0xC000)
    //
    cpu.registers.getReg(RegNames.A) should be(0x80)
    cpu.registers.isC should be(false)
    cpu.registers.isH should be(true)
    cpu.registers.isF3 should be(false)
    cpu.registers.isF5 should be(false)
    cpu.registers.isN should be(true)
  }


  "daa add" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x3e, 0x09)) // ld a,09
    loc = memory.setMemory(loc, Array(0xc6, 0x01)) // add a,01
    loc = memory.setMemory(loc, Array(0x27)) // daa
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.registers = cpu.registers.copy(regFile1 = cpu.registers.setFlags(cf = Option(false)))
    cpu.run(0xC000)
    //
    cpu.registers.getReg(RegNames.A) should be(0x10)
    cpu.registers.isC should be(false)
    cpu.registers.isH should be(true)
    cpu.registers.isF3 should be(false)
    cpu.registers.isF5 should be(false)
    cpu.registers.isN should be(false)
  }


  "daa add overflow" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x3e, 0x98)) // ld a,98
    loc = memory.setMemory(loc, Array(0xc6, 0x02)) // add a,02
    loc = memory.setMemory(loc, Array(0x27)) // daa
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.registers = cpu.registers.copy(regFile1 = cpu.registers.setFlags(cf = Option(false)))
    cpu.run(0xC000)
    //
    cpu.registers.getReg(RegNames.A) should be(0x00)
    cpu.registers.isC should be(true)
    cpu.registers.isH should be(true)
    cpu.registers.isF3 should be(false)
    cpu.registers.isF5 should be(false)
    cpu.registers.isN should be(false)
  }


  "daa sub" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x3e, 0x11)) // ld a,11
    loc = memory.setMemory(loc, Array(0xd6, 0x02)) // sub a,02
    loc = memory.setMemory(loc, Array(0x27)) // daa
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.registers = cpu.registers.copy(regFile1 = cpu.registers.setFlags(cf = Option(false)))
    cpu.run(0xC000)
    //
    cpu.registers.getReg(RegNames.A) should be(0x09)
    cpu.registers.isC should be(false)
    cpu.registers.isH should be(false)
    cpu.registers.isF3 should be(true)
    cpu.registers.isF5 should be(false)
    cpu.registers.isN should be(true)
  }

  "daa sub overflow" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x3e, 0x55)) // ld a,55
    loc = memory.setMemory(loc, Array(0xd6, 0x56)) // sub a,56
    loc = memory.setMemory(loc, Array(0x27)) // daa
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.registers = cpu.registers.copy(regFile1 = cpu.registers.setFlags(cf = Option(false)))
    cpu.run(0xC000)
    //
    cpu.registers.getReg(RegNames.A) should be(0x99)
    cpu.registers.isC should be(true)
    cpu.registers.isH should be(false)
    cpu.registers.isF3 should be(true)
    cpu.registers.isF5 should be(false)
    cpu.registers.isN should be(true)
  }


  "and" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x3e, 0xa5)) // ld a,a5
    loc = memory.setMemory(loc, Array(0xe6, 0x99)) // and a,99
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg(RegNames.A) should be(0x81)
    cpu.registers.isC should be(false)
    cpu.registers.isH should be(true)
    cpu.registers.isF3 should be(false)
    cpu.registers.isF5 should be(false)
    cpu.registers.isN should be(false)
  }


  "xor" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x3e, 0xa5)) // ld a,a5
    loc = memory.setMemory(loc, Array(0xee, 0x99)) // xor a,99
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg(RegNames.A) should be(0x3c)
    cpu.registers.isC should be(false)
    cpu.registers.isH should be(false)
    cpu.registers.isF3 should be(true)
    cpu.registers.isF5 should be(true)
    cpu.registers.isN should be(false)
    cpu.registers.isPV should be(true)
    cpu.registers.isZ should be(false)
    cpu.registers.isS should be(false)
  }


  "or" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x3e, 0xa5)) // ld a,a5
    loc = memory.setMemory(loc, Array(0xf6, 0x9b)) // or a,9b
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg(RegNames.A) should be(0xbf)
    cpu.registers.isC should be(false)
    cpu.registers.isH should be(false)
    cpu.registers.isF3 should be(true)
    cpu.registers.isF5 should be(true)
    cpu.registers.isN should be(false)
    cpu.registers.isPV should be(false)
    cpu.registers.isZ should be(false)
    cpu.registers.isS should be(true)
  }


  "cp" should "should change correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x3e, 0x55)) // ld a,a5
    loc = memory.setMemory(loc, Array(0xfe, 0x54)) // cp a,54
    //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg(RegNames.A) should be(0x55)
    cpu.registers.isC should be(false)
    cpu.registers.isH should be(true)
    cpu.registers.isF3 should be(false)
    cpu.registers.isF5 should be(false)
    cpu.registers.isN should be(true)
    cpu.registers.isPV should be(false)
    cpu.registers.isZ should be(false)
    cpu.registers.isS should be(false)
  }
}
