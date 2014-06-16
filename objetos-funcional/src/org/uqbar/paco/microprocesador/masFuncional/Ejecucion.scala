package org.uqbar.paco.microprocesador.masFuncional

object Ejecucion {

  trait ResultadoDeEjecucion
  case class Ejecutando(micro: Microprocesador) extends ResultadoDeEjecucion
  case class Halt(micro: Microprocesador) extends ResultadoDeEjecucion
  case class Error(micro: Microprocesador, descripcion: String) extends ResultadoDeEjecucion

  // ****************************************************************
  // ** EJECUTAR
  // ****************************************************************

  // Más funcionaloso: Evitemos el efecto colateral en el micro
  def ejecutar(micro: Microprocesador, programa: Instruccion*): ResultadoDeEjecucion = programa.toList match {
    case Nil => Ejecutando(micro)
    case instrucciones @ instruccionActual :: instruccionesRestantes =>
      val resultadoActual = instruccionActual match {
        case HALT => Halt(micro)

        case DIV if(micro.b == 0) => Error(micro, "Division por 0")

        case inst @ WHNZ(instruccionesInternas @_*) =>
          val pcInicial = micro.pc + instruccionActual.bytes
          if(micro.a != 0)
            ejecutar(micro.copy(pc = pcInicial), instruccionesInternas :_*) match {
              case Ejecutando(m) => ejecutar(
                  m.copy(pc = pcInicial - instruccionActual.bytes), 
                  instruccionActual
              )
              case otro => otro
            }
          else Ejecutando(micro.copy(pc = pcInicial + inst.bytesCuerpo))

        case inst @ IFNZ(instruccionesInternas @_*) =>
          if(micro.a != 0) ejecutar(micro pc_+= instruccionActual.bytes, instruccionesInternas: _*) match {
              case Ejecutando(m) => Ejecutando(m pc_+= END.bytes)
              case otro => otro
          }
          else Ejecutando(micro pc_+= inst.bytesCuerpo)

        case otra =>
          val siguienteMicro = otra match {
            case NOP => micro
            case ADD => micro.guardar(micro.a + micro.b)
            case SUB => micro.guardar(micro.a - micro.b)
            case MUL => micro.guardar(micro.a * micro.b)
            case DIV => micro.guardar(micro.a / micro.b)
            case SWAP => micro.copy(a = micro.b, b = micro.a)
            case LODV(valor) => micro.copy(a = valor)
            case LOD(direccion) => micro.copy(a = micro.memoriaDeDatos(direccion))
            case STR(direccion) => micro.copy(memoriaDeDatos = micro.memoriaDeDatos.updated(direccion, micro.a))
          }

          Ejecutando(siguienteMicro pc_+= instruccionActual.bytes)
      }

      resultadoActual match {
        case Ejecutando(m) => ejecutar(m,instruccionesRestantes :_*)
        case otro => otro
      }
  }
}