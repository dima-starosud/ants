package ants

import akka.typed.ActorRef
import cats.Monad
import cats.free.Free

final class ProcessDefToBehaviorCont[In] extends ProcessDef.Visitor[In, BehaviorCont[In, ?]] {
  override def self: BehaviorCont[In, ProcessId[In]] =
    Monad[BehaviorCont[In, ?]].map(BehaviorCont.self)(_.asInstanceOf[ProcessId[In]])

  override def receive: BehaviorCont[In, In] = BehaviorCont.receive

  override def cast[In1](pid: ProcessId[In1], message: In1): BehaviorCont[In, Unit] =
    BehaviorCont.cast(pid.asInstanceOf[ActorRef[In1]], message)

  override def spawn[In1](process: Free[ProcessDef[In1, ?], Nothing]): BehaviorCont[In, ProcessId[In1]] = {
    val behavior = process.foldMap(new ProcessDefToBehaviorCont[In1])
    Monad[BehaviorCont[In, ?]].map(BehaviorCont.spawn(behavior))(_.asInstanceOf[ProcessId[In1]])
  }

  override def stop: BehaviorCont[In, Bottom] = BehaviorCont.stop

  override def recur: BehaviorCont[In, Bottom] = BehaviorCont.recur
}
