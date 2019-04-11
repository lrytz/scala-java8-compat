package scala.compat.java8

package object collectionImpl {
  type Accumulator[A] = scala.jdk.AnyAccumulator[A]
  val Accumulator = scala.jdk.AnyAccumulator

  type IntAccumulator = scala.jdk.IntAccumulator
  val IntAccumulator = scala.jdk.IntAccumulator

  type LongAccumulator = scala.jdk.LongAccumulator
  val LongAccumulator = scala.jdk.LongAccumulator

  type DoubleAccumulator = scala.jdk.DoubleAccumulator
  val DoubleAccumulator = scala.jdk.DoubleAccumulator

  type Stepper[A] = scala.collection.Stepper[A]
  val Stepper = scala.collection.Stepper
}
