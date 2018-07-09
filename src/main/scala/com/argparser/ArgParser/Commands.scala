package com.argparser.ArgParser

import scalaz.Scalaz._
import scalaz._

import scala.collection.immutable.HashMap
import scala.language.{implicitConversions, reflectiveCalls}
import scala.util.control.Exception._
import scala.util.{Failure, Success, Try}

object Commands {
  implicit val ExceptionMonad: Monad[Try] = new Monad[Try] {
    override def point[A](a: => A): Try[A] = Try(a)

    override def bind[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = fa.flatMap(f)
  }


  private[this] def getOpts(v: Opts): List[Opt] = {
    val cls = v.getClass

    cls.getDeclaredFields.toList.withFilter(s => {
      failAsValue(classOf[NoSuchMethodException])(false) {
        cls.getDeclaredMethod(s.getName)
        true
      }
    }).map(f => {
      val n = f.getName
      val dv = cls.getMethod(n).invoke(v)
      Opt(n, dv)
    })
  }

  private def makeArgObj[O](ths: Object, m: HashMap[String, Object], arg: Class[_], opts: List[Opt]): O = {
    val is = arg.getConstructors()(0)
    val arr: List[AnyRef] = opts.map(o => m.getOrElse(o.name, o.default))
    val num = is.getParameterCount
    val aa2 = if (opts.size =/= num) ths :: arr else arr
    aa2.size assert_=== num
    is.newInstance(aa2: _*).asInstanceOf[O]
  }

  trait Opts
  trait GPhase[A, B] {
    type OO <: Opts
    lazy val help: String = {
      if (name =/= null && opts.nonEmpty) {
        val s = StringBuilder.newBuilder
        val desc = (!description.isEmpty) ? s": $description" | ""
        s ++= s" * Phase $name$desc\n"

        if (opts.nonEmpty) {
          opts.foreach(o => {
            optDescriptions.get(o.name) match {
              case Some(odesc) => s ++= s"  - ${o.name}: $odesc"
              case None => s ++= s"  - ${o.name}"
            }
            s ++= s" (default: ${o.default.toString})\n"
          })
        }

        s.toString()
      } else ""
    }
    protected lazy val _opts: List[Opt] = getOpts(arg)
    lazy val opts: List[Opt] = _opts
    val arg: Opts = EmptyO
    val name: String
    val description: String = ""
    val optDescriptions: HashMap[String, String] = HashMap.empty[String, String]

    final def _init(s: List[String], g: Opts, o: HashMap[String, Object]): Try[B] = {
      s |> init >>= _run(s, o, g)
    }

    def _run(gs: List[String], args: HashMap[String, Object], g: Opts)(s: A): Try[B] = {
      val localArg: OO = makeArgObj(this, args, arg.getClass, opts)
      run(gs, g, localArg, s)
    }

    def run(gs: List[String], g: Opts, o: OO, s: A): Try[B] = Failure(new Exception("run must be overridden."))
    def init(s: List[String]): Try[A] = throw new Exception("init must be overridden.")

    def >>[C](p2: GPhase[B, C]): GPhase[A, C] = PhaseList(this, p2)
  }

  trait UnitPhase extends GPhase[Unit, Unit] {
    override type OO = EmptyO.type

    override def init(s: List[String]): Try[Unit] = Try(())
    override def run(gs: List[String], g: Opts, o: EmptyO.type, e: Unit): Try[Unit] = Success(Unit)
  }

  trait InitPhase[A] extends GPhase[Unit, A] {
    override def init(s: List[String]): Try[Unit] = Try(())
  }

  abstract class Cmd(_phase: GPhase[_, _] = NilPhase) {
    final lazy val listOfOpts: List[Opt] = opts ++ phase.opts
    private lazy val opts: List[Opt] = getOpts(arg)
    final val phase: GPhase[_, _] = _phase
    lazy val help: String = {
      val s = StringBuilder.newBuilder
      opts.foreach(o => {
        s ++= s"- ${o.name}"
        optDescriptions.get(o.name).map { desc => s ++= s": $desc" }
        s ++= s" (default: ${o.default.toString})\n"
      })
      s.toString()
    }
    protected val arg: Opts = EmptyO
    val optDescriptions: HashMap[String, String] = HashMap.empty[String, String]
    val description: String = ""

    final def _init(s: List[String], o: HashMap[String, Object]): Try[_] = {
      val ths = failAsValue(classOf[NoSuchFieldException])(this) {
        arg.getClass.getField("$outer").get(arg)
      }
      val gOpts = makeArgObj[Opts](ths, o, arg.getClass, opts)
      phase._init(s, gOpts, o)
    }
  }

  final case class PhaseList[A, B, C](p1: GPhase[A, B], p2: GPhase[B, C]) extends GPhase[A, C] {
    override lazy val opts: List[Opt] = p1.opts ++ p2.opts
    override lazy val _opts: List[Opt] = List.empty[Opt]
    override lazy val help: String = p1.help + p2.help
    override val name: String = null

    override def init(s: List[String]): Try[A] = p1.init(s)

    override def _run(gs: List[String], m: HashMap[String, Object], g: Opts)(s: A): Try[C] = {
      s |> p1._run(gs, m, g) >>= p2._run(gs, m, g)
    }
  }

  /**
    * Default Phase
    *
    * @param desc a description of the application.
    */
  case class Help(desc: String) extends Cmd(new UnitPhase {
    override type OO = EmptyO.type
    override val name: String = "help"

    override def run(gs: List[String], g: Opts, o: EmptyO.type, e: Unit): Try[Unit] = {
      // a default behavior of help command.
      Cmd.map.foreach(s => {
        println(s"- Command: ${s._1} - ${s._2.description}")
        print(s._2.help)
        print(s._2.phase.help)
      })
      super.run(gs, g, o, e)
    }
  }) {
    override val description: String = "help command"
  }

  case object EmptyO extends Opts

  case object NilPhase extends GPhase[Unit, Unit] {
    override type OO = EmptyO.type
    override val name: String = null

    override def init(s: List[String]): Try[Unit] = Try(())
    override def run(gs: List[String], g: Opts, o: OO, s: Unit): Try[Unit] = Try(())
  }

  final case object UnitCmd extends Cmd(NilPhase)

  object Cmd {
    var map: HashMap[String, Cmd] = HashMap.empty[String, Cmd]
    var default: Cmd = UnitCmd

    def get(name: List[String]): (Cmd, List[String]) = {
      name.nonEmpty ? (map.getOrElse(name.head, default), name.tail) | (default, name)
    }
  }
}
