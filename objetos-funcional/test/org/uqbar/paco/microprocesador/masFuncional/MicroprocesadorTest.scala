package org.uqbar.paco.microprocesador.masFuncional

import org.junit.Assert._
import org.junit.Test
import org.uqbar.paco.microprocesador.masFuncional._
import org.uqbar.paco.microprocesador.masFuncional.Ejecucion._
import org.junit.Before
import org.junit.runner.RunWith
import org.junit.internal.runners.statements.Fail

class MicroprocesadorTest {

  var micro: Microprocesador = _
  var resultado: ResultadoDeEjecucion = _

   // ****************************************************************
  // ** TESTS
  // ****************************************************************

  @Test
  def `lodv escribe en el acumulador A` = {
    ejecutarEnMicro(
      LODV(255),
      HALT
    )

    assertRegistros(255,0)
    assertProgramCounter(2)
  }

  @Test
  def `swap` = {
    ejecutarEnMicro(
      LODV(15),
      SWAP,
      HALT
    )

    assertRegistros(0,15)
    assertProgramCounter(3)
  }

  @Test
  def `programa que suma uno mas uno` = {
    ejecutarEnMicro(
      LODV(1),
      ADD,
      LODV(1),
      ADD,
      HALT
    )

    assertRegistros(0,2)
    assertProgramCounter(6)
  }

  @Test
  def `halt dentro de un while` = {
    ejecutarEnMicro(
      LODV(1),
      WHNZ(
        HALT,
        LODV(2)
      ),
      LODV(3)
    )

    assertRegistros(1, 0)
  }

  @Test
  def `programa que escribe la memoria` = {
    ejecutarEnMicro(
      LODV(255),
      STR(333),
      HALT
    )

    assertRegistros(255,0)
    assertProgramCounter(5)
    assertDatos(255, 333)
  }

  @Test
  def `programa que lee la memoria` = {
    micro = micro.copy(memoriaDeDatos = micro.memoriaDeDatos.updated(333,145:Short))

    ejecutarEnMicro(
      LOD(333),
      HALT
    )

    assertRegistros(145,0)
    assertProgramCounter(3)
  }

  @Test
  def `If que ejecuta lo que esta dentro` = {
    ejecutarEnMicro(
      LODV(3),
      IFNZ(
        SWAP,
        LODV(1),
        ADD
      ),
      LODV(5),
      HALT
    )

    assertRegistros(5, 4)
    assertProgramCounter(10)
  }

  @Test
  def `If que NO ejecuta lo que esta dentro` = {
    ejecutarEnMicro(
      LODV(3),
      SWAP,
      IFNZ(
        LODV(5)
      ),
      HALT
    )

    assertRegistros(0,3)
    assertProgramCounter(6)
  }

  @Test
  def `WHNZ que cuenta hasta 3` = {
    ejecutarEnMicro(
      LODV(3),
      WHNZ(
        STR(10),
        LODV(1),
        SWAP,
        LOD(10),
        SUB,
        SWAP
      ),
      HALT
    )

    assertRegistros(0,0)
    assertProgramCounter(15)
  }

  @Test
  def `WHNZ que no entra nunca` = {
    ejecutarEnMicro(
      LODV(3),
      SWAP,
      WHNZ(
        LODV(5),
        NOP,
        NOP,
        NOP
        ),
      HALT)

    assertRegistros(0, 3)
    assertProgramCounter(10)
  }

  @Test
  def `HALT en medio de un programa` = {
    ejecutarEnMicro(
      LODV(3),
      HALT,
      LODV(5)
    )

    assertRegistros(3,0)
    assertProgramCounter(2)
  }

   // ****************************************************************
  // ** SET-UP & TEAR-DOWN
  // ****************************************************************

  @Before
  def before = {
    micro = new Microprocesador
    resultado = null
  }

  // ****************************************************************
  // ** ASSERTS
  // ****************************************************************

  def ejecutarEnMicro(programa: Instruccion*) = resultado = ejecutar(micro,programa :_*)

  def assertDatos(expected:Short, position: Int) = resultado match {
    case Halt(micro) =>
      assertEquals(expected, micro.memoriaDeDatos(position))
    case _ => fail
  }

  def assertRegistros(expectedA:Short, expectedB:Short) = {
    resultado match {
      case Halt(micro) =>
        assertEquals(expectedA, micro.a)
        assertEquals(expectedB, micro.b)
      case _ => fail
    }
  }

  def assertProgramCounter(expectedPc: Int) = resultado match {
    case Halt(micro) => assertEquals(expectedPc, micro.pc)
    case _ => fail
  }

}