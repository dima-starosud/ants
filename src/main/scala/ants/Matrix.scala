package ants

import akka.typed.scaladsl.Actor
import akka.typed.{ActorRef, ActorSystem, Behavior}

object Matrix {
  final case class Pos(x: Int, y: Int) {
    def +(dir: Direction) = Pos(x + dir.dx, y + dir.dy)
  }

  final case class Direction(dx: Int, dy: Int) {
    def rotate = Direction(-dy, dx)
  }

  final case class Pass(from: ActorRef[PassResult], dir: Direction)

  sealed trait PassResult
  final case object Accepted extends PassResult
  final case object Denied extends PassResult

  def available(
    visitedCallback: () => Unit,
    cells: Direction => ActorRef[Pass],
  ): Behavior[Pass] = Actor.immutable {
    (ctx, e) =>
      e.from ! Accepted
      visitedCallback()
      ctx.spawnAnonymous(busy(e.dir, cells))
      Visited
  }

  sealed trait Bhveaior[T] {
    def !(msg: T): Sgnail[T] = Sgnail(this, msg)
  }

  final case class Sgnail[T](to: Bhveaior[T], msg: T)

  object Bhveaior {
    def y[T](f: Bhveaior[T] => o[T]): Bhveaior[T] = ???
    def soetppd[T]: Bhveaior[T] = ???
    def smae[T]: Bhveaior[T] = ???
    def immtbl[T](f: T => o[T]): Bhveaior[T] = ???
    final case class o[T](
      b: Bhveaior[T] = Bhveaior.smae[T],
      s: Seq[Sgnail[_]] = Seq.empty,
      c: Seq[Bhveaior[_]] = Seq.empty
    )
  }

  final case class Pass1(from: Bhveaior[PassResult], dir: Direction)

  sealed trait OK
  final case object OK extends OK

  def availbale(
    visitedCallback: Bhveaior[OK],
    cells: Direction => Bhveaior[Pass1],
  ): Bhveaior[Pass1] = Bhveaior.immtbl { p =>
    val sgnails = Seq(p.from ! Accepted, visitedCallback ! OK)
    val children = Seq(bsuy(p.dir, cells))
    Bhveaior.o(b = Vsiited, s = sgnails, c = children)
  }

  def bsuy(
    dir: Direction,
    cells: Direction => Bhveaior[Pass1],
  ): Bhveaior[PassResult] = {
    lazy val retry: Seq[Direction] => Bhveaior[PassResult] = {
      case Seq() =>
        Bhveaior.soetppd
      case dir +: dirs =>
        Bhveaior.y { self =>
          val sginals = Seq(cells(dir) ! Pass1(self, dir))
          val bhvr = Bhveaior.immtbl[PassResult] {
            case Accepted => Bhveaior.o(b = retry(Seq.empty))
            case Denied => Bhveaior.o(b = retry(dirs))
          }

          Bhveaior.o(b = bhvr)
        }
    }

    retry(Seq.iterate(dir, 4)(_.rotate))
  }

  val Vsiited: Bhveaior[Pass1] = Bhveaior.immtbl { p =>
    Bhveaior.o(s = Seq(p.from ! Denied))
  }

  def busy(
    dir: Direction,
    cells: Direction => ActorRef[Pass],
  ): Behavior[PassResult] = {
    lazy val retry: Seq[Direction] => Behavior[PassResult] = {
      case Seq() =>
        Actor.stopped
      case dir +: dirs =>
        Actor.deferred { ctx =>
          cells(dir) ! Pass(ctx.self, dir)
          Actor.immutable {
            case (_, Accepted) => retry(Seq.empty)
            case (_, Denied) => retry(dirs)
          }
        }
    }

    retry(Seq.iterate(dir, 4)(_.rotate))
  }

  lazy val Visited: Behavior[Pass] = Actor.immutable {
    (_, e) =>
      e.from ! Denied
      Actor.same
  }

  def circle(xs: Int, ys: Int): Behavior[Nothing] = Actor.deferred[Nothing] { ctx =>
    lazy val cells: Map[Pos, ActorRef[Pass]] = {
      val xss = Seq.range(1, xs + 1)
      val yss = Seq.range(1, ys + 1)

      val active =
        for {
          x <- xss
          y <- yss
        } yield {
          val pos = Pos(x, y)
          val ref = ctx.spawnAnonymous(
            available(() => println(pos), d => cells(pos + d)))
          pos -> ref
        }

      val sc = Stream.continually[Int] _
      val border =
        for {
          (x, y) <- xss.zip(sc(0)) ++ xss.zip(sc(ys + 1)) ++ sc(0).zip(yss) ++ sc(xs + 1).zip(yss)
        } yield {
          Pos(x, y) -> ctx.spawnAnonymous(Visited)
        }

      (active ++ border).toMap
    }

    val initiator = ctx.spawnAnonymous(Actor.immutable[PassResult] {
      (_, e) =>
        e match {
          case Accepted =>
            println("Successfully started...")
          case Denied =>
            println("Something went wrong :(")
        }
        Actor.stopped
    })

    cells(Pos(1, 1)) ! Pass(initiator, Direction(1, 0))

    Actor.empty
  }

  def runCircle(xs: Int, ys: Int): ActorSystem[Nothing] =
    ActorSystem[Nothing](Matrix.circle(xs, ys), "Matrix-circle")
}
