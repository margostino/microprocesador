package org.uqbar.paco.microprocesador

import scala.language.implicitConversions

package object funcional {

  implicit def SeqToList[T](s:Seq[T]) = s.toList

  // ****************************************************************
  // ** EJECUTAR
  // ****************************************************************

  class HaltException extends Exception

  def ejecutar(micro: Microprocesador, programa: Instruccion*) = {
    def ejecutarR(programa: List[Instruccion]):Unit = programa match {
      case Nil =>
      case HALT :: _ => throw new HaltException
      case siguiente :: restantes =>
        siguiente match {
          case NOP =>
          case ADD => micro.guardar(micro.a + micro.b)
          case SUB => micro.guardar(micro.a - micro.b)
          case MUL => micro.guardar(micro.a * micro.b)
          case DIV => if(micro.b != 0) micro.guardar(micro.a / micro.b) else throw new Exception("Division por 0")

          case SWAP =>
            val temp = micro.a
            micro.a = micro.b
            micro.b = temp
          case LODV(valor) => micro.a = valor
          case LOD(direccion) => micro.a = micro.memoriaDeDatos(direccion)
          case STR(direccion) => micro.memoriaDeDatos(direccion) = micro.a
          case WHNZ(instrucciones @_*) =>
            val pcInicial = micro.pc + siguiente.bytes
            while(micro.a != 0) {
              micro.pc = pcInicial
              ejecutarR(instrucciones)
            }
            micro.pc = pcInicial + instrucciones.map(_.bytes).sum
          case IFNZ(instrucciones @_*) =>
            micro.pc += siguiente.bytes
            if(micro.a != 0) ejecutarR(instrucciones)
            else micro.pc += instrucciones.map(_.bytes).sum
        }

        micro.pc += siguiente.bytes
        ejecutarR(restantes)
    }

    try ejecutarR(programa.toList)
    catch {
      case e : HaltException =>
    }
  }

  // ****************************************************************
  // ** SIMPLIFICAR
  // ****************************************************************

  def simplificar(programa: Instruccion*): Seq[Instruccion] = programa.toList match {
    case Nil => Nil
    case NOP :: restantes => simplificar(restantes:_*)
    case SWAP :: SWAP::restantes => simplificar(restantes:_*)
    case LODV(_) :: LODV(x)::restantes => simplificar(LODV(x) :: restantes:_*)
    case LOD(_) :: LOD(x)::restantes => simplificar(LOD(x) :: restantes:_*)
    case STR(_) :: STR(x)::restantes => simplificar(STR(x) :: restantes:_*)
    case IFNZ() :: restantes => simplificar(restantes:_*)
    case IFNZ(instrucciones) :: restantes => IFNZ(simplificar(instrucciones):_*) :: simplificar(restantes:_*)
    case WHNZ(instrucciones) :: restantes => WHNZ(simplificar(instrucciones):_*) :: simplificar(restantes:_*)
    case instruccion :: restantes => instruccion :: simplificar(restantes:_*)
  }
}