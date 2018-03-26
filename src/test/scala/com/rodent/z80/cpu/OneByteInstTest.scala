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
}
