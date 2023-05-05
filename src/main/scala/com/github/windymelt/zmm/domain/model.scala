package com.github.windymelt.zmm.domain

import scala.concurrent.duration.FiniteDuration

package object model {
  import cats.data.Kleisli

  // TODO?: XML => Context

  /** Filter of Context. It takes Context, and returns Context with some
    * side-effect (usually, F is [[Seq]]).
    */
  type Filter[F[_]] = Kleisli[F, Context, Context]

  type VowelSeqWithDuration = Seq[(String, FiniteDuration)]
}
