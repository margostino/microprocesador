package org.uqbar.paco.microprocesador.funcional

abstract class Instruccion(val bytes: Int = 1)

case object NOP extends Instruccion
case object ADD extends Instruccion
case object SUB extends Instruccion
case object MUL extends Instruccion
case object DIV extends Instruccion
case object SWAP extends Instruccion
case object HALT extends Instruccion
case object HALTNZ extends Instruccion

case class LODV(valor: Short) extends Instruccion(2)
case class LOD(direccion: Int) extends Instruccion(3)
case class STR(direccion: Int) extends Instruccion(3)
case class WHNZ(instrucciones: Instruccion*) extends Instruccion
case class IFNZ(instrucciones: Instruccion*) extends Instruccion

object END {
  val bytes = 1
}