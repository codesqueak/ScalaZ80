package com.rodent.z80

package object CPU {

  object RegNames extends Enumeration {
    type RegName = Value
    val A, F, B, C, D, E, H, L, IX, IY, SP, PC, R, INST, M8, M16, ADDR, X, Y, Z, P, Q = Value
  }

}