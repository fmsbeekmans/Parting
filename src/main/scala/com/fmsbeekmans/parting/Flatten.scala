package com.fmsbeekmans.parting

import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}

trait Flatten[P <: Path, A] {
  type Out <: FlatKeyValues

  def apply(
    prefix: P,
    a: A
  ): Out
}

object Flatten {

  def apply[P <: Path, A](implicit inst: Flatten[P, A]): Flatten[P, A] = inst

  type Aux[P <: Path, A, O <: FlatKeyValues] = Flatten[P, A] { type Out = O }

  implicit def flattenString[P <: Path]: Flatten.Aux[P, String, FlatKeyValueCons[P, String, FlatKeyValueNil]] =
    new Flatten[P, String] {
      type Out = FlatKeyValueCons[P, String, FlatKeyValueNil]

      override def apply(
        prefix: P,
        a: String
      ): Out = FlatKeyValueCons(FlatKeyValue(prefix, a), FlatKeyValueNil)
    }

  implicit def flattenHNil[P <: Path]: Flatten.Aux[P, HNil, FlatKeyValueNil] =
    new Flatten[P, HNil] {
      type Out = FlatKeyValueNil

      override def apply(
        prefix: P,
        a: HNil
      ): Out = FlatKeyValueNil
    }

  implicit def flattenHCons[P <: Path, K <: Symbol, H, HO <: FlatKeyValues, T <: HList, TO <: FlatKeyValues, O <: FlatKeyValues](
    implicit
    witness: Witness.Aux[K],
    hFlatten: Lazy[Flatten.Aux[P / Field[K], H, HO]], // TODO Lazy, K :+ P
    tFlatten: Flatten.Aux[P, T, TO],
    concat: FlatKeyValues.Concat.Aux[HO, TO, O]
  ): Flatten.Aux[P, FieldType[K, H] :: T, O] =
    new Flatten[P, FieldType[K, H] :: T] {
      override type Out = O

      println(witness.value.name)

      override def apply(
        prefix: P,
        a: FieldType[K, H] :: T
      ): O = {
        val init: HO = hFlatten.value( /(prefix, Field[K]) , a.head)
        val tail: TO = tFlatten(prefix, a.tail)

        concat(init, tail)
      }
    }

  implicit def genericFlatten[P <: Path, A, H, O <: FlatKeyValues](
    implicit
    lGen: LabelledGeneric.Aux[A, H],
    flatten: Flatten.Aux[P, H, O]
  ): Flatten.Aux[P, A, O] =
    new Flatten[P, A] {
      type Out = O

      override def apply(
        prefix: P,
        a: A
      ): Out = flatten(prefix, lGen.to(a))
    }
}
