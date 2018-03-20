package com.rodent.z80.cpu

case class BaseRegisters(a: Int = 0, f: Int = 0, b: Int = 0, c: Int = 0, d: Int = 0, e: Int = 0, h: Int = 0, l: Int = 0,
                         data8: Option[Int] = None, data16: Option[Int] = None, wz: Option[Int] = None) {


}
