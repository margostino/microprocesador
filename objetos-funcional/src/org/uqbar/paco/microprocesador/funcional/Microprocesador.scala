package org.uqbar.paco.microprocesador.funcional

class Microprocesador {
  val memoriaDeDatos: Array[Short] = new Array(1024)
  var a: Short = 0
  var b: Short = 0
  var pc: Int = 0

  def guardar(valor: Int): Unit = {
    a = ((valor & 0xFF00) >> 4).toShort
    b = (valor & 0x00FF).toShort
  }
}