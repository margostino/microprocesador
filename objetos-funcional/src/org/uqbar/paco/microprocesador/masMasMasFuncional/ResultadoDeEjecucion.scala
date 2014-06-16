package org.uqbar.paco.microprocesador.masMasMasFuncional

trait ResultadoDeEjecucion {
  def micro: Microprocesador
  def map(f: Microprocesador => Microprocesador): ResultadoDeEjecucion
  def flatMap(f: Microprocesador => ResultadoDeEjecucion): ResultadoDeEjecucion
  def filter(f: Microprocesador => Boolean): ResultadoDeEjecucion
}

case class Ejecutando(micro: Microprocesador) extends ResultadoDeEjecucion {
  def map(f: Microprocesador => Microprocesador) = Ejecutando(f(micro))
  def flatMap(f: Microprocesador => ResultadoDeEjecucion) = f(micro)
  def filter(f: Microprocesador => Boolean) = if (f(micro)) this else Error("Ups...", micro)
}

case class Halt(micro: Microprocesador) extends ResultadoDeEjecucion {
  def map(f: Microprocesador => Microprocesador) = this
  def flatMap(f: Microprocesador => ResultadoDeEjecucion) = this
  def filter(f: Microprocesador => Boolean) = this
}

case class Error(descripcion: String, micro: Microprocesador) extends ResultadoDeEjecucion {
  def map(f: Microprocesador => Microprocesador) = this
  def flatMap(f: Microprocesador => ResultadoDeEjecucion) = this
  def filter(f: Microprocesador => Boolean) = this
}
