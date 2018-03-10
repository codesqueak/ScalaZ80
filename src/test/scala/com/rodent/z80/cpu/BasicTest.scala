package com.rodent.z80.cpu

import com.rodent.z80.io.{Memory, Ports}
import org.scalatest.{FlatSpec, Matchers}

class BasicTest extends FlatSpec with Matchers {


  def loadMemory(memory: Memory, filename: String): Memory = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close
    //
    for (line <- lines) {
      if (line.length > 1) {
        //    println("Loading: " + line)
        val parts = line.split(" ")
        val base = Integer.parseInt(parts(0), 16)
        for (i <- 0 to 7) memory.setMemory(base + i, Integer.parseInt(parts(i + 1), 16))
      }
    }
    memory
  }


  "Memory" should "reset correctly" in {
    var memory = new Memory()
    memory = loadMemory(memory, "NAS_Test.nas")
    // A very simple I/O routine to simulate NAS-SYS character output call
    memory.setMemory(0x30, 0xD3) // out (00), a
    memory.setMemory(0x31, 0x00) //
    memory.setMemory(0x32, 0xC9) // ret
    //
    val ports = new Ports()
    val cpu = new Processor(memory, ports, Registers())

    cpu.run(0x1000);
  }


  //
  //
  //  "CPU" should "reset correctly" in {
  //    val memory = new Memory()
  //    memory.setMemory(Array(0x00, 0x00, 0x76))
  //    val ports = new Ports()
  //    val cpu = new Processor(memory, ports, Registers())
  //    cpu.run()
  //
  //    cpu.registers.getReg16(RegNames.H) should be(0x0000)
  //
  //    println("HALT")
  //
  //  }

}