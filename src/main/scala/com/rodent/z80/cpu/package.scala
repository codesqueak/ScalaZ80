package com.rodent.z80

package object CPUZ {

  object RegNames extends Enumeration {
    type RegName = Value
    val A, F, B, C, D, E, H, L, IX, IY, IXIYH, IXIYL, SP, PC, R, INST, DATA8, DATA16, WZ, X, Y, Z, P, Q = Value
  }

  class RegInt(val v: Int) {
    // math
    def inc8: Int = (v + 1) & 0x00FF

    def dec8: Int = (v - 1) & 0x00FF

    def inc16: Int = (v + 1) & 0xFFFF

    def dec16: Int = (v - 1) & 0xFFFF

    def lsb: Int = v & 0x00FF;

    def msb: Int = (v & 0x00FF00) >>> 8

    def limit8: Int = v & 0x00FF

    def limit16: Int = v & 0xFFFF

    // flag status
    def f5: Option[Boolean] = Option((v & 0x20) != 0)

    def f3: Option[Boolean] = Option((v & 0x08) != 0)
  }

  implicit def regInt(v: Int): RegInt = new RegInt(v)

}