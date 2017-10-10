package com.rodent.z80

package object RegNames extends Enumeration {
  type RegName = Value
  val A, F, B, C, D, E, H, L, IX, IY, SP, PC, R, M8, M16 = Value
}