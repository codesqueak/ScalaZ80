package com.rodent.z80

package object CPUZ {

  object RegNames extends Enumeration {
    type RegName = Value
    val A, F, B, C, D, E, H, L, IX, IY, SP, PC, R, INST, M8, M16, ADDR, X, Y, Z, P, Q = Value
  }

  class RegInt(val v: Int) {
    // math
    def inc8: Int = (v + 1) & 0x00FF

    def dec8: Int = (v - 1) & 0x00FF

    def inc16: Int = (v + 1) & 0xFFFF

    def dec16: Int = (v - 1) & 0xFFFF

    def lsb: Int = v & 0x00FF;

    def msb: Int = (v & 0x00FF00) >>> 8

    // flag status
    def f5: Option[Boolean] = Some((v & 0x20) != 0)

    def f3: Option[Boolean] = Some((v & 0x08) != 0)
  }

  implicit def regInt(v: Int): RegInt = new RegInt(v)

}