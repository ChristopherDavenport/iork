package io.chrisdavenport.iork

import cats._
import cats.data._

final case class IorK[F[_], G[_], A](run: Ior[F[A], G[A]]){

  def mapK[H[_]](f: G ~> H): IorK[F, H, A] =
      IorK(run.map(f.apply))

  def fold[C](
    left: F[A] => C, 
    right: G[A] => C,
    both: (F[A], G[A]) => C
  ): C = run.fold(left, right, both)
}

object IorK {
  
  def leftc[F[_], G[_], A](x: F[A]): IorK[F, G, A] =
    IorK(Ior.left(x))

  def rightc[F[_], G[_], A](x: G[A]): IorK[F, G, A] =
    IorK(Ior.right(x))

  def bothc[F[_], G[_], A](l: F[A], r: G[A]): IorK[F, G, A] =
    IorK(Ior.both(l, r))
}