package com.fmsbeekmans.parting

import shapeless.Witness

sealed trait Path

sealed trait Root extends Path
case object Root extends Root {
  def /[S <: Segment](s: S): /[Root, S] = new /(Root, s)
}

case class /[+Parent <: Path, +S <: Segment](parent: Parent, segment: Segment) extends Path

// TODO sealed abstract case class?
sealed trait Segment extends Path
case class Field[Name <: Symbol]()(implicit witness: Witness.Aux[Name]) extends Segment {
  override def toString: String = s"Field[${witness.value.name}]"
}