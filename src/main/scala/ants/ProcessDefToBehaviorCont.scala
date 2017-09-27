package ants

import akka.typed.ActorRef
import cats.Monad
import cats.free.Free

final class ProcessDefToBehaviorCont[In] extends ProcessDef.Visitor[In, BehaviorCont[In, ?]] {
  override def self: BehaviorCont[In, ProcessId[In]] =
    Monad[BehaviorCont[In, ?]].map(BehaviorCont.self)(ProcessId(_))

  override def receive: BehaviorCont[In, In] = BehaviorCont.receive

  override def cast[In1](pid: ProcessId[In1], message: In1): BehaviorCont[In, Unit] =
    BehaviorCont.cast(pid.impl.asInstanceOf[ActorRef[In1]], message)

  override def spawn[In1](process: Free[ProcessDef[In1, ?], Nothing]): BehaviorCont[In, ProcessId[In1]] = {
    val behavior = process.foldMap(new ProcessDefToBehaviorCont[In1])
    Monad[BehaviorCont[In, ?]].map(BehaviorCont.spawn(behavior))(ProcessId(_))
  }

  override def stop: BehaviorCont[In, Nothing] = BehaviorCont.stop

  override def recur: BehaviorCont[In, Nothing] = BehaviorCont.recur
}