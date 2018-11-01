package io.chrisdavenport.iork

import cats._
import cats.data._
import cats.implicits._

final case class IorK[F[_], G[_], A](run: Ior[F[A], G[A]]){

  def mapK[H[_]](f: G ~> H): IorK[F, H, A] =
      IorK(run.map(f.apply))

  def isLeft: Boolean =
    run.isLeft

  def isRight: Boolean =
    run.isRight

  def swap: IorK[G, F, A] =
      IorK(run.swap)

  final def left: Option[F[A]] = run.left
  final def right: Option[G[A]] = run.right
  final def onlyLeft: Option[F[A]] = run.onlyLeft
  final def onlyRight: Option[G[A]] = run.onlyRight

  final def onlyLeftOrRight: Option[Either[F[A], G[A]]] = run.onlyLeftOrRight
  final def onlyBoth: Option[(F[A], G[A])] = run.onlyBoth
  final def pad: (Option[F[A]], Option[G[A]]) = run.pad
  final def unwrap: Either[Either[F[A], G[A]], (F[A], G[A])] =run.unwrap

  def foldMap[B](f: A => B)(implicit F: Foldable[F], G: Foldable[G], M: Monoid[B]): B =
      run.fold(F.foldMap(_)(f), G.foldMap(_)(f), (l, r) => l.foldMap(f).combine(r.foldMap(f)))
  
  def semiFold[C](
    left: F[A] => C, 
    right: G[A] => C,
    both: (F[A], G[A]) => C
  ): C = run.fold(left, right, both)

  def fold[H[_]](
    left: F ~> H,
    right: G ~> H,
    swivel: Tuple2K[F, G, ?] ~> H
  ): H[A] = run.fold(left(_), right(_), (l, r) => swivel(Tuple2K(l, r)))

  final def putLeft[H[_]](left: H[A]): IorK[H, G, A] =
    semiFold(_ => IorK.leftc(left), IorK.bothc(left, _), (_, b) => IorK.bothc(left, b))
  final def putRight[H[_]](right: H[A]): IorK[F, H, A] =
    semiFold(IorK.bothc(_, right), _ => IorK.rightc(right), (a, _) => IorK.bothc(a, right))
}

object IorK {
  
  def leftc[F[_], G[_], A](x: F[A]): IorK[F, G, A] =
    IorK(Ior.left(x))

  def rightc[F[_], G[_], A](x: G[A]): IorK[F, G, A] =
    IorK(Ior.right(x))

  def bothc[F[_], G[_], A](l: F[A], r: G[A]): IorK[F, G, A] =
    IorK(Ior.both(l, r))
}