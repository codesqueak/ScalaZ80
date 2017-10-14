package com.rodent.z80.func

import com.rodent.z80.func.Utils.{toHex16, toHex8}
import org.scalatest._

class UtilsTest extends FlatSpec with Matchers {

  "A 16 bit hex string" should "should have four digits" in {
    val stack = toHex16(0)
    stack should be("0000")
  }

  "An 8 bit hex string" should "should have two digits" in {
    val stack = toHex8(0)
    stack should be("00")
  }

  "A 16 bit integer should be encoded correctly" should "should have four hex digits" in {
    val stack = toHex16(65535)
    stack should be("ffff")
  }

  "An 8 bit integer should be encoded correctly" should "should have two hex digits" in {
    val stack = toHex8(255)
    stack should be("ff")
  }
}