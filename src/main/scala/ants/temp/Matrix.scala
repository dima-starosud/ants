package ants.temp

object Matrix {

  import akka.typed.scaladsl.Actor
  import akka.typed.{ActorRef, ActorSystem, Behavior}

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
