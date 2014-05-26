package de.sciss.contextsnake.txn

import scala.swing.{Label, MainFrame, BoxPanel, Orientation, ProgressBar, Frame, Button, Graphics2D, Rectangle, Component, SimpleSwingApplication, Swing}
import de.sciss.lucre.stm.InMemory
import Swing._
import scala.collection.mutable
import scala.collection.immutable.{IndexedSeq => Vec}
import java.awt.geom.GeneralPath
import java.awt.Color
import scala.swing.event.{MouseDragged, MouseReleased, MousePressed}
import scala.concurrent.{ExecutionContext, Future, blocking}
import de.sciss.lucre.geom.{IntRectangle, IntPoint2D, IntSpace, IntSquare}
import de.sciss.kollflitz
import de.sciss.lucre.stm
import de.sciss.numbers.Implicits._
import ExecutionContext.Implicits.global
import de.sciss.swingplus.Spinner
import javax.swing.SpinnerNumberModel
import de.sciss.lucre.data.SkipOctree
import scala.annotation.tailrec
import de.sciss.serial.{DataInput, DataOutput, Serializer}

object FuzzyApp extends SimpleSwingApplication {
  type S = InMemory
  type D = IntSpace.TwoDim
  val system = InMemory()

  val DEBUG = false

  val sz = 512

  // val dataCube = IntSquare(sz >> 1, sz >> 1, sz >> 1)
  val dataCube = IntSquare(0, 0, sz)

  def point(x: Int, y: Int): Point = Point(x.clip(0, sz - 1), y.clip(0, sz -1))

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
      val pred  = if (isEmpty) p else point(pred0.getX.toInt, pred0.getY.toInt)
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

  def produceFuzzy(tree: ContextTree[S, D, Move], corpus: SkipOctree[S, D, Move], body0: Vec[Move],
                   singleChoice0: Int, maxSingleChoice: Int = 2, fuzzyAmount: Int = 1)
             (implicit tx: S#Tx, random: TxnRandom[S#Tx]): (Int, Option[Vec[Move]]) = {
    var singleChoice = singleChoice0

    def mkFuzzy(in: Vec[Move]): Vec[IntRectangle] = {
      val si = in.size
      Vec.tabulate(si) { i =>
        val f   = ((si - (i + 1)) * fuzzyAmount).min(sz)
        val m   = in(i)
        val ext = 2 * f + 1
        IntRectangle(m.x - f, m.y - f, ext, ext)
      }
    }

    def collect1(in: Vec[Move], range: IntRectangle): Vec[Vec[Move]] = {
      val succ = tree.snake(in).successorsRange(range)
      succ.map(in :+ _).toIndexedSeq
    }

    def collect2(in: Vec[Move]): Vec[Vec[Move]] = {
      val succ = tree.snake(in).successors
      succ.map(in :+ _).toIndexedSeq
    }

    @tailrec def collect(res: Vec[Vec[Move]], in: Vec[IntRectangle]): Vec[Vec[Move]] = in match {
      case head +: tail =>
        val newBodies = res.flatMap { body =>
          collect1(body, head)
        }
        collect(newBodies, tail)

      case _ =>
        res.flatMap { body =>
          collect2(body)
        }
    }

    var snake = body0

    while (snake.nonEmpty) {
      val fuzzy   = mkFuzzy(snake)
      val heads   = corpus.rangeQuery(fuzzy.head).toIndexedSeq
      val headsB  = heads.map(Vec(_))
      val sq      = collect(headsB, fuzzy.tail)
      val sz      = sq.size
      val ssz     = snake.size
      if (DEBUG) println(s"Snake size = $ssz, succ size = $sz, singleChoice = $singleChoice")
      if (ssz > 1 && (sz == 0 || sz == 1 && singleChoice >= maxSingleChoice)) {
        snake = snake.tail
        if (DEBUG) println("...trim start")
      } else {
        val elem = if (sz == 1) {
          if (DEBUG) println("...head")
          singleChoice += 1
          sq.head
        } else {
          if (DEBUG) println(s"...rnd($sz)")
          singleChoice = 0
          val idx = (random.nextDouble() * sz).toInt
          sq(idx)
        }
        // this might be a problem: mkFuzzy will be applied to something that was already shifted
        // snake /* :+ */ = elem
        return (singleChoice, Some(elem))
      }
    }
    (singleChoice, None)
  }

  def produce(snake: ContextTree.Snake[S, D, Move], singleChoice0: Int, maxSingleChoice: Int = 2)
             (implicit tx: S#Tx, random: TxnRandom[S#Tx]): (Int, Option[Move]) = {
    var singleChoice = singleChoice0

    def REFRESH(): Unit = {
      val b = snake.iterator.toList
      if (DEBUG) println(s"(preserved body size is ${b.size})")
      snake.CLEAR()
      snake.appendAll(b)
      if (DEBUG) println(s"(restored body size is ${snake.size})")
    }

    while (snake.nonEmpty) {
      val sq  = snake.successors.toIndexedSeq // .sorted
      val sz  = sq.size
      val ssz = snake.size
      if (DEBUG) println(s"Snake size = $ssz, succ size = $sz, singleChoice = $singleChoice")
      if (ssz > 1 && (sz == 0 || sz == 1 && singleChoice >= maxSingleChoice)) {
        snake.trimStart(1)
        if (DEBUG) println("...trim start")
        REFRESH()
      } else {
        val elem = if (sz == 1) {
          if (DEBUG) println("...head")
          singleChoice += 1
          sq.head
        } else {
          if (DEBUG) println(s"...rnd($sz)")
          singleChoice = 0
          val idx = (random.nextDouble() * sz).toInt
          sq(idx)
        }
        snake += elem
        return (singleChoice, Some(elem))
      }
    }
    (singleChoice, None)
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
        ggTrain.append(point(pt.x, pt.y))
      case MouseReleased(_, _, _, _, _) =>
      case MouseDragged (_, pt, _) =>
        ggTrain.append(point(pt.x, pt.y))
    }

    //    var anim = Option.empty[(stm.Source[S#Tx, ContextTree.Snake[S, D, Move]],
    //                             stm.Source[S#Tx, TxnRandom.Writable[S]])]

    @volatile var animCnt = 0

    val singleModel = new SpinnerNumberModel(2, 2, 256, 1)
    val ggSingle    = new Spinner(singleModel)

    val fuzzyModel  = new SpinnerNumberModel(0, 0, 64, 1)
    val ggFuzzy     = new Spinner(fuzzyModel)

    ggGen.listenTo(ggGen.mouse.clicks)
    ggGen.reactions += {
      case MousePressed(_, pt, _, _, _) =>
        ggGen.clear()
        val ctxV  = ctxHFut.value
        val singleMax = singleModel.getNumber.intValue()
        val hOpt  = ctxV.flatMap(_.toOption).flatMap { ctxH =>
          system.step { implicit tx =>
            val ctx = ctxH()
            // println(s"tree size = ${ctx.size}")
            if (ctx.nonEmpty) {
              val rnd     = TxnRandom[S](System.currentTimeMillis()) // (1234L)
              val move1   = ctx.apply(rnd.nextInt(ctx.length)) // (ctx.length - 1)
              val _snake  = ctx.snake(move1 :: Nil)
              val snakeH  = tx.newHandle(_snake)
              val rndH    = tx.newHandle(rnd)
              Some((move1, snakeH, rndH, ctxH))
            } else None
          }
        }

        // println(s"ctxV = $ctxV; snakeHOpt = $snakeHOpt")

        // anim = hOpt.map { case (m0, snakeH, rndH) => (snakeH, rndH) }

        hOpt.foreach {
          case (m0, snakeH, rndH, ctxH) =>
            val p0 = point(pt.x, pt.y)
            val p1 = point(p0.x + m0.x, p0.y + m0.y)
            ggGen.append(p0)
            ggGen.append(p1)

            val animCnt0 = animCnt

            val fuzzyAmt  = fuzzyModel.getNumber.intValue()
            val useFuzzy  = fuzzyAmt > 0

            Future {
              blocking {
                var singleChoice = 0

                var corpusH: stm.Source[S#Tx, SkipOctree[S, D, Move]] = null
                var body1: Vec[Move] = null

                while (animCnt == animCnt0) {
                  val t0 = System.currentTimeMillis()
                  val (_s, moveOpt) = if (useFuzzy) {
                    val (__s, _bodyOutOpt) = system.step { implicit tx =>
                      implicit val rnd = rndH()
                      val ctx = ctxH()
                      val corpus = if (corpusH == null) {
                        implicit def viewMove(m: Move, tx: S#Tx): D#PointLike = m
                        val oct = SkipOctree.empty[S, D, Move](dataCube)
                        ctx.iterator.foreach { move =>
                          oct += move
                        }
                        implicit val octSer = new Serializer[S#Tx, S#Acc, SkipOctree[S, D, Move]] {
                          def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): SkipOctree[S, D, Move] =
                            SkipOctree.read[S, D, Move](in, access)

                          def write(v: SkipOctree[S, D, Move], out: DataOutput): Unit = v.write(out)
                        }
                        corpusH = tx.newHandle(oct)
                        oct

                      } else {
                        corpusH()
                      }

                      val body = if (body1 == null) {
                        snakeH().iterator.toIndexedSeq

                      } else body1

                      println(s"Body size = ${body.size}; singleChoice = $singleChoice")

                      produceFuzzy(ctx, corpus = corpus, body0 = body, singleChoice0 = singleChoice,
                        maxSingleChoice = singleMax, fuzzyAmount = fuzzyAmt)
                    }
                    _bodyOutOpt.foreach(b => body1 = b)
                    (__s, _bodyOutOpt.map(_.last))

                  } else {
                    system.step { implicit tx =>
                      val snake = snakeH()
                      implicit val rnd = rndH()
                      produce(snake, singleChoice0 = singleChoice, maxSingleChoice = singleMax)
                    }
                  }
                  singleChoice = _s
                  // println(moveOpt)
                  moveOpt.foreach { move =>
                    Swing.onEDT {
                      ggGen.lastPointOption.foreach { pred =>
                        val succ = point(pred.x + move.x,
                                         pred.y + move.y)
                        ggGen.append(succ)
                      }
                    }
                  }
                  val t1 = System.currentTimeMillis()
                  val dt = 25 - (t1 - t0)
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
      val moves = pts.mapPairs((pred, succ) => Move(succ.x - pred.x, succ.y - pred.y))

      ctxHFut = Future {
        blocking {
          system.step { implicit tx =>
            val moves1 = if (moves.isEmpty) moves else moves :+ moves.head  // avoid element with no successors
            val ctx = ContextTree[S, D, Move](dataCube)(moves1: _*)
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
            contents += Swing.HStrut(8)
            contents += new Label("Max Single:")
            contents += ggSingle
            contents += Swing.HStrut(8)
            contents += new Label("Fuzziness:")
            contents += ggFuzzy
            contents += Swing.HGlue
          }
        }
      }
    }
  }
}
