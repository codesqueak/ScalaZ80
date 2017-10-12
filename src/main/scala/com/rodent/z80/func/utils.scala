package com.rodent.z80.func

import scala.runtime.RichInt

// Varoius util functions

class utils {}

object utils {

  def toHex16(v: Int): String = {
    val hex = "0000" + new RichInt(v).toHexString;
    hex.substring(hex.length - 4)
  }

  def toHex8(v: Int): String = {
    val hex = "00" + new RichInt(v).toHexString;
    hex.substring(hex.length - 2)
  }

}
