package org.uqbar.paco.microprocesador

import scala.language.implicitConversions

package object masMasFuncional {

  implicit def SeqToList[T](s:Seq[T]) = s.toList

  def simplificar(programa: Instruccion*): Seq[Instruccion] = programa.toList match {
    case Nil => Nil
    case IFNZ() :: restantes => simplificar(restantes:_*)
    case IFNZ(instrucciones) :: restantes => IFNZ(simplificar(instrucciones):_*) :: simplificar(restantes:_*)
    case NOP :: restantes => simplificar(restantes:_*)
    case SWAP :: SWAP::restantes => simplificar(restantes:_*)
    case LODV(_) :: LODV(x)::restantes => simplificar(LODV(x) :: restantes:_*)
    case LOD(_) :: LOD(x)::restantes => simplificar(LOD(x) :: restantes:_*)
    case STR(_) :: STR(x)::restantes => simplificar(STR(x) :: restantes:_*)
    case instruccion :: restantes => instruccion :: simplificar(restantes:_*)
  }
}