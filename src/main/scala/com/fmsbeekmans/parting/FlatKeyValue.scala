package com.fmsbeekmans.parting

case class FlatKeyValue[P <: Path, V](
  flatKey: P,
  value: V
)

sealed trait FlatKeyValues extends Product with Serializable

sealed trait FlatKeyValueNil extends FlatKeyValues
case object FlatKeyValueNil extends FlatKeyValueNil

case class FlatKeyValueCons[P <: Path, V, +Tail <: FlatKeyValues](
  head: FlatKeyValue[P, V],
  tail: Tail
) extends FlatKeyValues

object FlatKeyValues {

  trait Concat[A <: FlatKeyValues, B <: FlatKeyValues] {
    type Out <: FlatKeyValues

    def apply(
      a: A,
      b: B
    ): Out
  }

  object Concat extends LowPriorityConcatInstances {
    type Aux[A <: FlatKeyValues, B <: FlatKeyValues, O <: FlatKeyValues] = Concat[A, B] { type Out = O }

    def apply[A <: FlatKeyValues, B <: FlatKeyValues](implicit inst: Concat[A, B]) = inst
    def aux[A <: FlatKeyValues, B <: FlatKeyValues, O <: FlatKeyValues](implicit inst: Concat.Aux[A, B, O]): Aux[A, B, O] = inst

    implicit def concatOntoNil[A <: FlatKeyValues]: Concat.Aux[A, FlatKeyValueNil, A] =
      new Concat[A, FlatKeyValueNil] {
        type Out = A

        override def apply(
          a: A,
          b: FlatKeyValueNil
        ): Out = a
      }
  }

  trait LowPriorityConcatInstances {

    implicit def concatNil[B <: FlatKeyValues]: Concat.Aux[FlatKeyValueNil, B, B] =
      new Concat[FlatKeyValueNil, B] {
        type Out = B

        override def apply(
          a: FlatKeyValueNil,
          b: B
        ): Out = b
      }

    implicit def concatCons[P <: Path, V, A <: FlatKeyValues, B <: FlatKeyValues, O <: FlatKeyValues](
      implicit
      concat: Concat.Aux[A, B, O]
    ): Concat.Aux[FlatKeyValueCons[P, V, A], B, FlatKeyValueCons[P, V, O]] =
      new Concat[FlatKeyValueCons[P, V, A], B] {
        override type Out = FlatKeyValueCons[P, V, O]

        override def apply(
          a: FlatKeyValueCons[P, V, A],
          b: B
        ): FlatKeyValueCons[P, V, O] = {
          FlatKeyValueCons(a.head, concat(a.tail, b))
        }
      }
  }
}
