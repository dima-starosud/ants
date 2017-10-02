package ants

import akka.typed.scaladsl.Actor
import akka.typed.{ActorRef, Behavior}
import cats.Monad

final case class BehaviorCont[In, A](
  cont: (A => Behavior[In]) => Behavior[In]
) extends ((A => Behavior[In]) => Behavior[In]) {
  override def apply(k: A => Behavior[In]): Behavior[In] = cont(k)

  def behavior(implicit ev: A =:= Nothing): Behavior[In] = cont(identity)
}

object BehaviorCont {
  final case class ContActorRef[In] private[BehaviorCont](
    private[BehaviorCont] val impl: ActorRef[In]
  ) {
    def ![In0](msg: In): BehaviorCont[In0, Unit] = BehaviorCont.cast(this, msg)
  }

  implicit def behaviorContMonad[In]: Monad[BehaviorCont[In, ?]] = new BehaviorContMonad[In]

  final class BehaviorContMonad[In] extends Monad[BehaviorCont[In, ?]] {
    override def pure[A](x: A): BehaviorCont[In, A] = BehaviorCont(_ (x))

    override def flatMap[A, B](
      fa: BehaviorCont[In, A])(f: A => BehaviorCont[In, B]
    ): BehaviorCont[In, B] = BehaviorCont(k => fa(f(_)(k)))

    override def tailRecM[A, B](
      a: A)(f: A => BehaviorCont[In, Either[A, B]]
    ): BehaviorCont[In, B] = BehaviorCont(k => f(a)(_.fold(tailRecM(_)(f)(k), k)))
  }

  def from[In](behavior: Behavior[In]): BehaviorCont[In, Unit] =
    BehaviorCont(Function.const(behavior))

  def self[In]: BehaviorCont[In, ContActorRef[In]] = BehaviorCont { k =>
    Actor.deferred(ctx => k(ContActorRef(ctx.self)))
  }

  def receive[In]: BehaviorCont[In, In] = BehaviorCont { k =>
    Actor.immutable((_, e) => k(e))
  }

  def cast[In, In1](actorRef: ContActorRef[In1], message: In1): BehaviorCont[In, Unit] = BehaviorCont { k =>
    Actor.deferred(_ => k(actorRef.impl ! message))
  }

  def spawn[In, In1](
    cont: BehaviorCont[In1, Bottom]
  ): BehaviorCont[In, ContActorRef[In1]] = BehaviorCont { k =>
    Actor.deferred(ctx => k(ContActorRef(ctx.spawnAnonymous(cont.behavior))))
  }

  def stop[In]: BehaviorCont[In, Bottom] =
    BehaviorCont(Function.const(Actor.stopped))

  def recur[In]: BehaviorCont[In, Bottom] =
    BehaviorCont(Function.const(Actor.same))

  trait LazyProxy[T] {
    type Proxy <: T
    def proxy: Proxy
    def fill(proxy: Proxy, value: T): Unit
  }

  def mfix[In, T](f: T => BehaviorCont[In, T])(implicit lp: LazyProxy[T]): BehaviorCont[In, T] = {
    BehaviorCont[In, T] { k =>
      val proxy = lp.proxy
      f(proxy).cont { value =>
        lp.fill(proxy, value)
        k(value)
      }
    }
  }
}
