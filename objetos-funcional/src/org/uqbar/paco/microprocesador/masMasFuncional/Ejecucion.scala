package org.uqbar.paco.microprocesador.masMasFuncional

object Ejecucion {

  trait ResultadoDeEjecucion
  case class Ejecutando(micro: Microprocesador) extends ResultadoDeEjecucion
  case class Halt(micro: Microprocesador) extends ResultadoDeEjecucion
  case class Error(micro: Microprocesador, descripcion: String) extends ResultadoDeEjecucion

  // Más declarativo. Sacamos la recursividad y usamos un fold!
  def ejecutar(micro: Microprocesador, programa: Instruccion*): ResultadoDeEjecucion =
    programa.foldLeft(Ejecutando(micro): ResultadoDeEjecucion) {
      case (Ejecutando(m), instruccion) =>
        val micro = m pc_+= instruccion.bytes
        instruccion match {
          case HALT => Halt(m)

          case DIV if (micro.b == 0) => Error(micro, "Division por 0")

          case inst @ WHNZ(instruccionesInternas @ _*) =>
            if (micro.a != 0) ejecutar(micro, instruccionesInternas: _*) match {
              case Ejecutando(mic) => ejecutar(mic.copy(pc = m.pc), WHNZ(instruccionesInternas: _*))
              case otro => otro
            }
            else Ejecutando(micro pc_+= inst.bytesCuerpo)

          case inst @ IFNZ(instruccionesInternas @ _*) =>
            if (micro.a != 0) ejecutar(micro, instruccionesInternas: _*) match {
              case Ejecutando(m) => Ejecutando(m pc_+= END.bytes)
              case otro => otro
            }
            else Ejecutando(micro pc_+= inst.bytesCuerpo)

          case NOP => Ejecutando(micro)
          case ADD => Ejecutando(micro.guardar(micro.a + micro.b))
          case SUB => Ejecutando(micro.guardar(micro.a - micro.b))
          case MUL => Ejecutando(micro.guardar(micro.a * micro.b))
          case DIV => Ejecutando(micro.guardar(micro.a / micro.b))
          case SWAP => Ejecutando(micro.copy(a = micro.b, b = micro.a))
          case LODV(valor) => Ejecutando(micro.copy(a = valor))
          case LOD(direccion) => Ejecutando(micro.copy(a = micro.memoriaDeDatos(direccion)))
          case STR(direccion) => Ejecutando(micro.copy(memoriaDeDatos = micro.memoriaDeDatos.updated(direccion, micro.a)))
        }
      case (otro, _) => otro
    }

  // ****************************************************************
  // ** OTRA OPERACIÓN QUE PODRḮAMOS HACER
  // ****************************************************************

  def primeraQueAccedaA(direccion: Int, instrucciones: Instruccion*): Option[Instruccion] =
    instrucciones.foldLeft(None: Option[Instruccion]) {
      case (res, inst @ (LOD(`direccion`) | STR(`direccion`))) =>
        res.fold(Some(inst))(Some(_))
      case (res, IFNZ(is @ _*)) =>
        res.orElse(primeraQueAccedaA(direccion, is: _*))
      case (res, WHNZ(is @ _*)) =>
        res.orElse(primeraQueAccedaA(direccion, is: _*))
      case (res, _) => res
    }

}