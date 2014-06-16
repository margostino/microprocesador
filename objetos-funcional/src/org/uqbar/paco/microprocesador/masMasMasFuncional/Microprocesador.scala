package org.uqbar.paco.microprocesador.masMasMasFuncional

case class Microprocesador(a: Short = 0, b: Short = 0, memoriaDeDatos: List[Short] = (1 to 1024).map(i => 0:Short), pc: Int = 0) {
  def pc_+=(inc: Int) = copy(pc = pc + inc)

  def guardar(valor: Int) = copy(
    a = ((valor & 0xFF00) >> 4).toShort,
    b = (valor & 0x00FF).toShort
  )
}