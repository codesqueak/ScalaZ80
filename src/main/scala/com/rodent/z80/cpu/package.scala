package com.rodent.z80

package object CPU {

  object RegNames extends Enumeration {
    type RegName = Value
    val A, F, B, C, D, E, H, L, IX, IY, SP, PC, R, INST, M8, M16, ADDR, X, Y, Z, P, Q = Value
  }

  class RegInt(val v: Int) {
    def inc8: Int = (v + 1) & 0x00FF

    def dec8: Int = (v - 1) & 0x00FF

    def inc16: Int = (v + 1) & 0xFFFF

    def dec16: Int = (v - 1) & 0xFFFF

    def lsb: Int = v & 0x00FF;

    def msb: Int = (v & 0x00FF00) >>> 8
  }

  implicit def regInt(v: Int): RegInt = new RegInt(v)

}