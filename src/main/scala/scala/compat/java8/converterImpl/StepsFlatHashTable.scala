/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.compat.java8.converterImpl

import scala.annotation.switch

import scala.compat.java8.collectionImpl._
import scala.compat.java8.runtime._

import Stepper._

/////////////////////////////
// Stepper implementations //
/////////////////////////////

private[java8] class StepsAnyFlatHashTable[A](_underlying: Array[AnyRef], _i0: Int, _iN: Int)
extends StepsLikeGapped[A, StepsAnyFlatHashTable[A]](_underlying, _i0, _iN) {
  def next() = if (currentEntry eq null) throwNSEE else { val ans = currentEntry.asInstanceOf[A]; currentEntry = null; ans }
  protected def semiclone(half: Int) = new StepsAnyFlatHashTable[A](underlying, i0, half)
}

private[java8] class StepsDoubleFlatHashTable(_underlying: Array[AnyRef], _i0: Int, _iN: Int)
extends StepsDoubleLikeGapped[StepsDoubleFlatHashTable](_underlying, _i0, _iN) {
  def nextDouble() = if (currentEntry eq null) throwNSEE else { val ans = currentEntry.asInstanceOf[Double]; currentEntry = null; ans }
  protected def semiclone(half: Int) = new StepsDoubleFlatHashTable(underlying, i0, half)
}

private[java8] class StepsIntFlatHashTable(_underlying: Array[AnyRef], _i0: Int, _iN: Int)
extends StepsIntLikeGapped[StepsIntFlatHashTable](_underlying, _i0, _iN) {
  def nextInt() = if (currentEntry eq null) throwNSEE else { val ans = currentEntry.asInstanceOf[Int]; currentEntry = null; ans }
  protected def semiclone(half: Int) = new StepsIntFlatHashTable(underlying, i0, half)
}

private[java8] class StepsLongFlatHashTable(_underlying: Array[AnyRef], _i0: Int, _iN: Int)
extends StepsLongLikeGapped[StepsLongFlatHashTable](_underlying, _i0, _iN) {
  def nextLong() = if (currentEntry eq null) throwNSEE else { val ans = currentEntry.asInstanceOf[Long]; currentEntry = null; ans }
  protected def semiclone(half: Int) = new StepsLongFlatHashTable(underlying, i0, half)
}
