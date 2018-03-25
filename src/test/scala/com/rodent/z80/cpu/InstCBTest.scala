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
}
