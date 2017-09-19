package ants.interpreter.akkatyped

import akka.typed.ActorRef
import ants.free.{ActorId, BehaviorOp}
import ants.interpreter.akkatyped.BehaviorCont.behaviorContMonad
import cats.Monad
import cats.free.Free

final class BehaviorOpToBehaviorCont[In] extends BehaviorOp.Visitor[In, BehaviorCont[In, ?]] {
  override def self: BehaviorCont[In, ActorId[In]] =
    Monad[BehaviorCont[In, ?]].map(BehaviorCont.self)(ActorId(_))

  override def receive: BehaviorCont[In, In] = BehaviorCont.receive

  override def cast[In1](actorRef: ActorId[In1], message: In1): BehaviorCont[In, Unit] =
    BehaviorCont.cast(actorRef.impl.asInstanceOf[ActorRef[In1]], message)

  override def spawn[In1](behaviorOp: Free[BehaviorOp[In1, ?], Nothing]): BehaviorCont[In, ActorId[In1]] = {
    val behavior = behaviorOp.foldMap(new BehaviorOpToBehaviorCont[In1])
    Monad[BehaviorCont[In, ?]].map(BehaviorCont.spawn(behavior))(ActorId(_))
  }

  override def stop: BehaviorCont[In, Nothing] = BehaviorCont.stop

  override def recur: BehaviorCont[In, Nothing] = BehaviorCont.recur
}
