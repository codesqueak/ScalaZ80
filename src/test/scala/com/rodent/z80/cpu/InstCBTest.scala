package com.rodent.z80.cpu

import com.rodent.z80.CPUZ.RegNames
import com.rodent.z80.io.{Memory, Ports}
import org.scalatest.{FlatSpec, Matchers}

class InstCBTest extends FlatSpec with Matchers {

  "RLC" should "execute correctly" in {
    var memory = new Memory()
    var loc = 0xC000

    loc = memory.setMemory(loc, Array(0x06)) // LD B
    loc = memory.setMemory(loc, Array(0x81)) //
    loc = memory.setMemory(loc, Array(0xCB)) // RLC B
    loc = memory.setMemory(loc, Array(0x00)) //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.B) should be(0x0300)
    cpu.registers.isC should be(true)
  }

  "RRC" should "execute correctly" in {
    var memory = new Memory()
    var loc = 0xC000

    loc = memory.setMemory(loc, Array(0x06)) // LD B
    loc = memory.setMemory(loc, Array(0x03)) //
    loc = memory.setMemory(loc, Array(0xCB)) // RRC B
    loc = memory.setMemory(loc, Array(0x08)) //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.B) should be(0x8100)
    cpu.registers.isC should be(true)
  }

  "RR" should "execute correctly" in {
    var memory = new Memory()
    var loc = 0xC000

    loc = memory.setMemory(loc, Array(0x06)) // LD B
    loc = memory.setMemory(loc, Array(0x03)) //
    loc = memory.setMemory(loc, Array(0xCB)) // RR B
    loc = memory.setMemory(loc, Array(0x18)) //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.B) should be(0x0100)
    cpu.registers.isC should be(true)
  }

  "SLA" should "execute correctly" in {
    var memory = new Memory()
    var loc = 0xC000

    loc = memory.setMemory(loc, Array(0x06)) // LD B
    loc = memory.setMemory(loc, Array(0x81)) //
    loc = memory.setMemory(loc, Array(0xCB)) // SLA B
    loc = memory.setMemory(loc, Array(0x20)) //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.B) should be(0x0200)
    cpu.registers.isC should be(true)
  }

  "SRA" should "execute correctly" in {
    var memory = new Memory()
    var loc = 0xC000

    loc = memory.setMemory(loc, Array(0x06)) // LD B
    loc = memory.setMemory(loc, Array(0x81)) //
    loc = memory.setMemory(loc, Array(0xCB)) // SLA B
    loc = memory.setMemory(loc, Array(0x28)) //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.B) should be(0xC000)
    cpu.registers.isC should be(true)
  }

  "SLL" should "execute correctly" in {
    var memory = new Memory()
    var loc = 0xC000

    loc = memory.setMemory(loc, Array(0x06)) // LD B
    loc = memory.setMemory(loc, Array(0x81)) //
    loc = memory.setMemory(loc, Array(0xCB)) // SLL B
    loc = memory.setMemory(loc, Array(0x30)) //
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.B) should be(0x0300)
    cpu.registers.isC should be(true)
  }


  "rlc (hl)" should "execute correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x21, 0x02, 0x10)) // ld ix,1002
    loc = memory.setMemory(loc, Array(0xcb, 0x06)) // rlc
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    memory.setMemory(0x1000, Array(0x01, 0x02, 0x83, 0x04, 0x05))
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.H) should be(0x1002)
    cpu.registers.isC should be(true)
    //
    memory.getMemory(0x1000) should be(0x01)
    memory.getMemory(0x1001) should be(0x02)
    memory.getMemory(0x1002) should be(0x07)
    memory.getMemory(0x1003) should be(0x04)
  }

  "rrc (hl)" should "execute correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x21, 0x02, 0x10)) // ld hl,1002
    loc = memory.setMemory(loc, Array(0xcb, 0x0e)) // rrc
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    memory.setMemory(0x1000, Array(0x01, 0x02, 0x83, 0x04, 0x05))
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.H) should be(0x1002)
    cpu.registers.isC should be(true)
    //
    memory.getMemory(0x1000) should be(0x01)
    memory.getMemory(0x1001) should be(0x02)
    memory.getMemory(0x1002) should be(0xc1)
    memory.getMemory(0x1003) should be(0x04)
  }

  "rl (hl)" should "execute correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x21, 0x02, 0x10)) // ld hl,1002
    loc = memory.setMemory(loc, Array(0xcb, 0x16)) // rl
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    memory.setMemory(0x1000, Array(0x01, 0x02, 0x73, 0x04, 0x05))
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.H) should be(0x1002)
    cpu.registers.isC should be(false)
    //
    memory.getMemory(0x1000) should be(0x01)
    memory.getMemory(0x1001) should be(0x02)
    memory.getMemory(0x1002) should be(0xe6)
    memory.getMemory(0x1003) should be(0x04)
  }

  "rr (hl)" should "execute correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x21, 0x02, 0x10)) // ld ihl,1002
    loc = memory.setMemory(loc, Array(0xcb, 0x1e)) // rr
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    memory.setMemory(0x1000, Array(0x01, 0x02, 0x83, 0x04, 0x05))
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.H) should be(0x1002)
    cpu.registers.isC should be(true)
    //
    memory.getMemory(0x1000) should be(0x01)
    memory.getMemory(0x1001) should be(0x02)
    memory.getMemory(0x1002) should be(0x41)
    memory.getMemory(0x1003) should be(0x04)
  }


  "sla (hl)" should "execute correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x21, 0x02, 0x10)) // ld hl,1002
    loc = memory.setMemory(loc, Array(0xcb, 0x26)) // sla
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    memory.setMemory(0x1000, Array(0x01, 0x02, 0x83, 0x04, 0x05))
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.H) should be(0x1002)
    cpu.registers.isC should be(true)
    //
    memory.getMemory(0x1000) should be(0x01)
    memory.getMemory(0x1001) should be(0x02)
    memory.getMemory(0x1002) should be(0x06)
    memory.getMemory(0x1003) should be(0x04)
  }

  "sra (hl)" should "execute correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x21, 0x02, 0x10)) // ld hl,1002
    loc = memory.setMemory(loc, Array(0xcb, 0x2e)) // sra
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    memory.setMemory(0x1000, Array(0x01, 0x02, 0x83, 0x04, 0x05))
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.H) should be(0x1002)
    cpu.registers.isC should be(true)
    //
    memory.getMemory(0x1000) should be(0x01)
    memory.getMemory(0x1001) should be(0x02)
    memory.getMemory(0x1002) should be(0xc1)
    memory.getMemory(0x1003) should be(0x04)
  }

  "sll (hl)" should "execute correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x21, 0x02, 0x10)) // ld hl,1002
    loc = memory.setMemory(loc, Array(0xcb, 0x36)) // sll
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    memory.setMemory(0x1000, Array(0x01, 0x02, 0xc3, 0x04, 0x05))
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.H) should be(0x1002)
    cpu.registers.isC should be(true)
    //
    memory.getMemory(0x1000) should be(0x01)
    memory.getMemory(0x1001) should be(0x02)
    memory.getMemory(0x1002) should be(0x87)
    memory.getMemory(0x1003) should be(0x04)
  }


  "srl (hl)" should "execute correctly" in {
    var memory = new Memory()
    var loc = memory.setMemory(0xC000, Array(0x21, 0x02, 0x10)) // ld hl,1002
    loc = memory.setMemory(loc, Array(0xcb, 0x3e)) // srl
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt
    memory.setMemory(0x1000, Array(0x01, 0x02, 0xc3, 0x04, 0x05))
    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.H) should be(0x1002)
    cpu.registers.isC should be(true)
    //
    memory.getMemory(0x1000) should be(0x01)
    memory.getMemory(0x1001) should be(0x02)
    memory.getMemory(0x1002) should be(0x61)
    memory.getMemory(0x1003) should be(0x04)
  }

}
