package de.sciss.contextsnake.txn

import scala.swing.{MainFrame, BoxPanel, Orientation, ProgressBar, Frame, Button, Graphics2D, Rectangle, Component, SimpleSwingApplication, Swing}
import de.sciss.lucre.stm.InMemory
import Swing._
import scala.collection.mutable
import scala.collection.immutable.{IndexedSeq => Vec}
import java.awt.geom.GeneralPath
import java.awt.Color
import scala.swing.event.{MouseDragged, MouseReleased, MousePressed}
import scala.concurrent.{ExecutionContext, Future, blocking}
import de.sciss.lucre.geom.{IntPoint2D, IntSpace, IntSquare}
import de.sciss.kollflitz
import de.sciss.lucre.stm
import de.sciss.numbers.Implicits._
import ExecutionContext.Implicits.global

object FuzzyApp extends SimpleSwingApplication {
  type S = InMemory
  type D = IntSpace.TwoDim
  val system = InMemory()

  val sz = 512

  // val dataCube = IntSquare(sz >> 1, sz >> 1, sz >> 1)
  val dataCube = IntSquare(0, 0, sz)

  case class Point(x: Int, y: Int) {
    require (x >= 0 && x < sz)
    require (y >= 0 && y < sz)
  }

  // case class Move(dx: Int, dy: Int)
  type Move = IntPoint2D
  val  Move = IntPoint2D

  import IntSpace.TwoDim.pointSerializer

  class Canvas extends Component {
    preferredSize = (sz, sz)
    minimumSize   = preferredSize
    maximumSize   = preferredSize
    border        = Swing.MatteBorder(1, 1, 1, 1, Color.lightGray)
    
    private val _pts  = mutable.Buffer.empty[Point]
    private val gp    = new GeneralPath()

    def points: Vec[Point] = _pts.toIndexedSeq

    def lastPointOption: Option[Point] = _pts.lastOption
    
    def clear(): Unit = {
      _pts.clear()
      gp.reset()
      repaint()
    }
    
    def append(p: Point): Unit = {
      _pts += p
      val pred0   = gp.getCurrentPoint
      val isEmpty = pred0 == null
      if (isEmpty) gp.moveTo(p.x, p.y) else gp.lineTo(p.x, p.y)
      val pred  = if (isEmpty) p else Point(pred0.getX.toInt, pred0.getY.toInt)
      val dx0   = pred.x min p.x
      val dx1   = pred.x max p.x
      val dy0   = pred.y min p.y
      val dy1   = pred.y max p.y
      repaint(new Rectangle(dx0 - 1, dy0 - 1, dx1 - dx0 + 2, dy1 - dy0 + 2))
    }

    override protected def paintComponent(g: Graphics2D): Unit = {
      g.setColor(Color.white)
      val w = peer.getWidth
      val h = peer.getHeight
      g.fillRect(0, 0, w, h)
      g.setColor(Color.black)
      g.draw(gp)
    }
  }

  def produce(snake: ContextTree.Snake[S, D, Move], maxSingleChoice: Int = 2)
             (implicit tx: S#Tx, random: TxnRandom[S#Tx]): Option[Move] = {
    var singleChoice = 0
    while (snake.nonEmpty) {
      val sq = snake.successors.toIndexedSeq // .sorted
      val sz = sq.size
      if (sz == 0 || sz == 1 && singleChoice == maxSingleChoice) {
        snake.trimStart(1)
      } else {
        val elem = if (sz == 1) {
          singleChoice += 1
          sq.head
        } else {
          singleChoice = 0
          val idx = (random.nextDouble() * sz).toInt
          sq(idx)
        }
        snake += elem
        return Some(elem)
      }
    }
    None
  }

  def mkClearButton(canvas: Canvas): Button = Button("Clear") {
    canvas.clear()
  }

  case class NoCorpus() extends Exception

  private var ctxHFut: Future[stm.Source[S#Tx, ContextTree[S, D, Move]]] = Future.failed(NoCorpus())

  lazy val top: Frame = {
    val ggTrain       = new Canvas
    val ggClearTrain  = mkClearButton(ggTrain)
    val ggGen         = new Canvas
    // val ggClearGen    = mkClearButton(ggGen)

    ggTrain.listenTo(ggTrain.mouse.clicks)
    ggTrain.listenTo(ggTrain.mouse.moves )
    ggTrain.reactions += {
      case MousePressed(_, pt, _, _, _) =>
        ggTrain.append(Point(pt.x, pt.y))
      case MouseReleased(_, _, _, _, _) =>
      case MouseDragged (_, pt, _) =>
        ggTrain.append(Point(pt.x, pt.y))
    }

    //    var anim = Option.empty[(stm.Source[S#Tx, ContextTree.Snake[S, D, Move]],
    //                             stm.Source[S#Tx, TxnRandom.Writable[S]])]

    @volatile var animCnt = 0

    ggGen.listenTo(ggGen.mouse.clicks)
    ggGen.reactions += {
      case MousePressed(_, pt, _, _, _) =>
        ggGen.clear()
        val ctxV  = ctxHFut.value
        val hOpt  = ctxV.flatMap(_.toOption).flatMap { ctxH =>
          system.step { implicit tx =>
            val ctx = ctxH()
            // println(s"tree size = ${ctx.size}")
            if (ctx.nonEmpty) {
              val move1   = ctx.apply(ctx.length - 1)
              val _snake  = ctx.snake(move1 :: Nil)
              val snakeH  = tx.newHandle(_snake)
              val rnd     = TxnRandom[S](1234L)
              val rndH    = tx.newHandle(rnd)
              Some((move1, snakeH, rndH))
            } else None
          }
        }

        // println(s"ctxV = $ctxV; snakeHOpt = $snakeHOpt")

        // anim = hOpt.map { case (m0, snakeH, rndH) => (snakeH, rndH) }

        hOpt.foreach {
          case (m0, snakeH, rndH) =>
            val p0 = Point(pt.x.clip(0, sz - 1), pt.y.clip(0, sz - 1))
            val p1 = Point((p0.x + m0.x).clip(0, sz - 1), (p0.y + m0.y).clip(0, sz - 1))
            ggGen.append(p0)
            ggGen.append(p1)

            val animCnt0 = animCnt

            Future {
              blocking {
                while (animCnt == animCnt0) {
                  val t0 = System.currentTimeMillis()
                  val moveOpt = system.step { implicit tx =>
                    val snake = snakeH()
                    implicit val rnd = rndH()
                    produce(snake)
                  }
                  println(moveOpt)
                  moveOpt.foreach { move =>
                    Swing.onEDT {
                      ggGen.lastPointOption.foreach { pred =>
                        val succ = Point((pred.x + move.x).clip(0, sz - 1),
                                         (pred.y + move.y).clip(0, sz - 1))
                        ggGen.append(succ)
                      }
                    }
                  }
                  val t1 = System.currentTimeMillis()
                  val dt = 100 - (t1 - t0)
                  if (dt > 0) Thread.sleep(dt)
                }
              }
              // println("RELEASE")
            }
        }

      case MouseReleased(_, _, _, _, _) => animCnt += 1
    }

    val ggBusy = new ProgressBar {
      // indeterminate = true
      preferredSize = (32, 24)
      minimumSize   = preferredSize
      maximumSize   = preferredSize
    }

    lazy val ggCorpus: Button = Button("Train") {
      val pts = ggTrain.points
      import kollflitz.Ops._
      val moves = pts.pairMap((pred, succ) => Move(succ.x - pred.y, succ.y - pred.y))

      ctxHFut = Future {
        blocking {
          system.step { implicit tx =>
            val ctx = ContextTree[S, D, Move](dataCube)(moves: _*)
            // println(s"After appending ${moves.size} values, context tree size is ${ctx.size}")
            println(s"context tree size is ${ctx.size}")
            tx.newHandle(ctx)
          }
        }
      }

      ggBusy.indeterminate  = true
      ggCorpus.enabled      = false

      ctxHFut.onComplete { _ =>
        Swing.onEDT {
          ggBusy.indeterminate  = false
          ggCorpus.enabled      = true
        }
      }

      // ...
    }

    new MainFrame {
      title     = "Drawing Snake"
      contents  = new BoxPanel(Orientation.Horizontal) {
        contents += new BoxPanel(Orientation.Vertical) {
          contents += ggTrain
          contents += new BoxPanel(Orientation.Horizontal) {
            contents += ggClearTrain
            contents += Swing.HGlue
          }
        }
        contents += new BoxPanel(Orientation.Vertical) {
          contents += ggGen
          contents += new BoxPanel(Orientation.Horizontal) {
            // contents += ggClearGen
            // contents += Swing.HStrut(16)
            contents += ggBusy
            contents += ggCorpus
            contents += Swing.HGlue
          }
        }
      }
    }
  }
}
