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


  "adc hl,rr" should "calculate correctly" in {
    var memory = new Memory()
    var loc = 0xC000
    loc = memory.setMemory(loc, Array(0x21, 0x22, 0x11)) // ld hl,1122
    loc = memory.setMemory(loc, Array(0x11, 0x44, 0x33)) // ld de,3344
    loc = memory.setMemory(loc, Array(0x01, 0x66, 0x55)) // ld bc,5566
    loc = memory.setMemory(loc, Array(0x31, 0x88, 0x77)) // ld sp,7788
    loc = memory.setMemory(loc, Array(0xED, 0x6A)) // adc hl,hl
    loc = memory.setMemory(loc, Array(0xED, 0x7A)) // adc hl,sp
    loc = memory.setMemory(loc, Array(0xED, 0x4A)) // adc hl,bc
    loc = memory.setMemory(loc, Array(0x37)) // scf
    loc = memory.setMemory(loc, Array(0xED, 0x5A)) // adc hl,de
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt

    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.B) should be(0x5566)
    cpu.registers.getReg16(RegNames.D) should be(0x3344)
    cpu.registers.getReg16(RegNames.SP) should be(0x7788)

    val result = (0x1122 + 0x1122 + 0x7788 + 0x5566 + 0x3344 + 1) & 0xFFFF

    cpu.registers.getReg16(RegNames.H) should be(result)
    cpu.registers.isC should be(true)

  }


  "sbc hl,rr" should "calculate correctly" in {
    var memory = new Memory()
    var loc = 0xC000
    loc = memory.setMemory(loc, Array(0x21, 0x34, 0x7F)) // ld hl,7f34
    loc = memory.setMemory(loc, Array(0x11, 0x55, 0x10)) // ld de,1055
    loc = memory.setMemory(loc, Array(0x01, 0x66, 0x10)) // ld bc,1066
    loc = memory.setMemory(loc, Array(0x31, 0x77, 0x10)) // ld sp,1077
    loc = memory.setMemory(loc, Array(0xED, 0x72)) // sbc hl,sp
    loc = memory.setMemory(loc, Array(0xED, 0x42)) // sbc hl,bc
    loc = memory.setMemory(loc, Array(0x37)) // scf
    loc = memory.setMemory(loc, Array(0xED, 0x52)) // sbc hl,de
    memory.setMemory(loc, Array(0x76, 0x76, 0x76, 0x76, 0x76, 0x76, 0x76)) // halt

    //
    // Ok, run the program
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())
    cpu.run(0xC000)
    //
    cpu.registers.getReg16(RegNames.B) should be(0x1066)
    cpu.registers.getReg16(RegNames.D) should be(0x1055)
    cpu.registers.getReg16(RegNames.SP) should be(0x1077)

    val result = (0x7f34 - 0x1077 - 0x1066 - 0x1055 - 1) & 0xFFFF

    cpu.registers.getReg16(RegNames.H) should be(result)
    cpu.registers.isC should be(false)
  }
}
