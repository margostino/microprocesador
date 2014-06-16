package org.uqbar.paco.microprocesador.masMasMasFuncional

trait Ejecucion {
  def ejecutar(micro: Microprocesador, programa: Instruccion*): ResultadoDeEjecucion
}

// ****************************************************************
// ** V1
// ****************************************************************

object Ejecucion1 extends Ejecucion {
  def ejecutar(micro: Microprocesador, programa: Instruccion*): ResultadoDeEjecucion =
    programa.foldLeft(Ejecutando(micro): ResultadoDeEjecucion) { (resultado, instruccion) =>
      val resultadoSiguiente = resultado.map { _ pc_+= instruccion.bytes }

      instruccion match {
        case HALT => resultado.flatMap { m => Halt(m) }

        case DIV if resultadoSiguiente.micro.b == 0 => resultadoSiguiente.flatMap { m => Error("Division por 0", m) }

        case inst @ WHNZ(instruccionesInternas @ _*) => resultadoSiguiente.flatMap {
          case m if m.a != 0 =>
            ejecutar(m, instruccionesInternas: _*).flatMap { micro =>
              ejecutar(micro.copy(pc = m.pc - instruccion.bytes), WHNZ(instruccionesInternas: _*))
            }
          case m => Ejecutando(m pc_+= inst.bytesCuerpo)
        }

        case inst @ IFNZ(instruccionesInternas @ _*) => resultadoSiguiente.flatMap {
          case m if m.a != 0 => ejecutar(m, instruccionesInternas: _*).map { m => m pc_+= 1 }
          case m => Ejecutando(m pc_+= inst.bytesCuerpo)
        }

        case NOP => resultadoSiguiente.map { m => m }
        case ADD => resultadoSiguiente.map { m => m.guardar(m.a + m.b) }
        case SUB => resultadoSiguiente.map { m => m.guardar(m.a - m.b) }
        case MUL => resultadoSiguiente.map { m => m.guardar(m.a * m.b) }
        case DIV => resultadoSiguiente.map { m => m.guardar(m.a / m.b) }
        case SWAP => resultadoSiguiente.map { case Microprocesador(a, b, mem, pc) => Microprocesador(b, a, mem, pc) }
        case LODV(valor) => resultadoSiguiente.map { case Microprocesador(_, b, mem, pc) => Microprocesador(valor, b, mem, pc) }
        case LOD(direccion) => resultadoSiguiente.map { case Microprocesador(_, b, mem, pc) => Microprocesador(mem(direccion), b, mem, pc) }
        case STR(direccion) => resultadoSiguiente.map { case Microprocesador(a, b, mem, pc) => Microprocesador(a, b, mem.updated(direccion, a), pc) }
      }
    }
}

// ****************************************************************
// ** V2
// ****************************************************************

object Ejecucion2 extends Ejecucion {
  def ejecutar(micro: Microprocesador, programa: Instruccion*): ResultadoDeEjecucion =
    programa.foldLeft(Ejecutando(micro): ResultadoDeEjecucion) { (resultado, instruccion) =>
      val resultadoSiguiente = resultado.map { _ pc_+= instruccion.bytes }

      instruccion match {
        case HALT => Halt(resultado.micro)

        case DIV => for {
          micro <- resultadoSiguiente
          if micro.b != 0
        } yield micro

        case inst @ WHNZ(instruccionesInternas @ _*) => for {
          micro <- resultadoSiguiente
          microPostInternas <- ejecutar(micro, instruccionesInternas: _*)
          microPostWhile <- ejecutar(microPostInternas.copy(pc = micro.pc - instruccion.bytes),
            (if (micro.a != 0) WHNZ(instruccionesInternas: _*) :: Nil else Nil): _*)
        } yield if (micro.a != 0) microPostWhile else micro pc_+= inst.bytesCuerpo

        case inst @ IFNZ(instruccionesInternas @ _*) => for {
          micro <- resultadoSiguiente
          microPostInternas <- ejecutar(micro, instruccionesInternas: _*)
        } yield if (micro.a == 0) micro pc_+= inst.bytesCuerpo else microPostInternas pc_+= 1

        case inst => for (micro <- resultadoSiguiente) yield inst match {
          case NOP => micro
          case ADD => micro.guardar(micro.a + micro.b)
          case SUB => micro.guardar(micro.a - micro.b)
          case MUL => micro.guardar(micro.a * micro.b)
          case SWAP => micro.copy(a = micro.b, b = micro.a)
          case LODV(valor) => micro.copy(a = valor)
          case LOD(direccion) => micro.copy(a = micro.memoriaDeDatos(direccion))
          case STR(direccion) => micro.copy(memoriaDeDatos = micro.memoriaDeDatos.updated(direccion, micro.a))
        }
      }
    }
}