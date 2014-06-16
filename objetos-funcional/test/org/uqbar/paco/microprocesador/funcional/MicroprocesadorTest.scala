package org.uqbar.paco.microprocesador.funcional

import org.junit.Assert.assertEquals
import org.junit.Test
import org.uqbar.paco.microprocesador.funcional._
import org.junit.Before
import org.junit.runner.RunWith

class MicroprocesadorTest {

  var micro: Microprocesador = _

  // ****************************************************************
  // ** TESTS
  // ****************************************************************

  @Test
  def `lodv escribe en el acumulador A` = {
    ejecutar(micro,
      LODV(255),
      HALT)

    assertRegistros(255, 0)
    assertProgramCounter(2)
  }

  @Test
  def `swap` = {
    ejecutar(micro,
      LODV(15),
      SWAP,
      HALT)

    assertRegistros(0, 15)
    assertProgramCounter(3)
  }

  @Test
  def `programa que suma uno mas uno` = {
    ejecutar(micro,
      LODV(1),
      ADD,
      LODV(1),
      ADD,
      HALT)

    assertRegistros(0, 2)
    assertProgramCounter(6)
  }

  @Test
  def `halt dentro de un while` = {
    ejecutar(micro,
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
    ejecutar(micro,
      LODV(255),
      STR(333),
      HALT)

    assertRegistros(255, 0)
    assertProgramCounter(5)
    assertEquals(255, micro.memoriaDeDatos(333))
  }

  @Test
  def `programa que lee la memoria` = {
    micro.memoriaDeDatos(333) = 145

    ejecutar(micro,
      LOD(333),
      HALT)

    assertRegistros(145, 0)
    assertProgramCounter(3)
  }

  @Test
  def `If que ejecuta lo que esta dentro` = {
    ejecutar(micro,
      LODV(3),
      IFNZ(
        SWAP,
        LODV(1),
        ADD),
      LODV(5),
      HALT)

    assertRegistros(5, 4)
    assertProgramCounter(10)
  }

  @Test
  def `If que NO ejecuta lo que esta dentro` = {
    ejecutar(micro,
      LODV(3),
      SWAP,
      IFNZ(
        LODV(5)),
      HALT)

    assertRegistros(0, 3)
    assertProgramCounter(7)
  }

  @Test
  def `WHNZ que cuenta hasta 3` = {
    ejecutar(micro,
      LODV(3),
      WHNZ(
        STR(10),
        LODV(1),
        SWAP,
        LOD(10),
        SUB,
        SWAP),
      HALT)

    assertRegistros(0, 0)
    assertProgramCounter(15)
  }

  @Test
  def `WHNZ que no entra nunca` = {
    ejecutar(micro,
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
    ejecutar(micro,
      LODV(3),
      HALT,
      LODV(5))

    assertRegistros(3, 0)
    assertProgramCounter(2)
  }

  // ****************************************************************
  // ** SET-UP & TEAR-DOWN
  // ****************************************************************

  @Before
  def before = micro = new Microprocesador

  // ****************************************************************
  // ** ASSERTS
  // ****************************************************************

  def assertRegistros(expectedA: Short, expectedB: Short) = {
    assertEquals(expectedA, micro.a)
    assertEquals(expectedB, micro.b)
  }

  def assertProgramCounter(expectedPc: Int) = assertEquals(expectedPc, micro.pc)
}