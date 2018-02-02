package com.danielasfregola.tutorial.cat.monad

import com.danielasfregola.tutorial.cat._
import com.danielasfregola.tutorial.cat.applicative.ApplicativeInstances._

// See solution at https://gist.github.com/DanielaSfregola/ddf48f6c5638f6284b563798c55d5ebd

object MonadInstances {

  implicit val maybeMonad: Monad[Maybe] = new Monad[Maybe] {
    override def flatMap[A, B](boxA: Maybe[A])(f: A => Maybe[B]): Maybe[B] = boxA match {
      case Just(a) => f(a)
      case _ => Empty
    }

    override def pure[A](a: A): Maybe[A] = Just(a)
  }

  implicit val zeroOrMoreMonad: Monad[ZeroOrMore] = new Monad[ZeroOrMore] {
    override def flatMap[A, B](boxA: ZeroOrMore[A])(f: A => ZeroOrMore[B]): ZeroOrMore[B] = boxA match {
      case OneOrMore(a, as) => f(a).append(flatMap(as)(f))
      case _ => Zero
    }

    override def pure[A](a: A): ZeroOrMore[A] = OneOrMore(a, Zero)
  }

}
