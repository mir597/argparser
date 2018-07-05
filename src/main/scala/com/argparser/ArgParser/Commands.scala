package com.argparser.ArgParser

import scala.collection.immutable.HashMap
import scala.language.{implicitConversions, reflectiveCalls}
import scala.util.control.Exception._
import scala.util.{Failure, Success, Try}

object Commands {
  implicit def any2waterfall[A](a: A): Object {def |>[B](f: A => B): B} = new AnyRef {
    def |>[B](f: A => B): B = f(a)
  }

  implicit def try2waterfall[U, A](a: Try[A]): Object {def |>>(f: A => Try[U]): Try[U]} = new AnyRef {
    def |>>(f: A => Try[U]): Try[U] = a.flatMap(f)
  }

  def getOpts(v: Opts): List[Opt] = {
    val cls = v.getClass
    val fs = cls.getDeclaredFields.toList.filter(s => {
      failAsValue(classOf[NoSuchMethodException])(false) {
        cls.getDeclaredMethod(s.getName)
        true
      }
    })
    fs.map(f => {
      val n = f.getName
      val m = cls.getMethod(n)
      val dv = m.invoke(v)
      f.getType.toString match {
        case "boolean" =>
          OSimple(n, Boolean.box(dv.asInstanceOf[Boolean]))
        case "int" =>
          OIntArg(n, Int.box(dv.asInstanceOf[Int]))
        case "class java.lang.String" =>
          OStrArg(n, dv.asInstanceOf[String])
        case o =>
          throw new InternalError(s"We only supports boolean, integer, and string values as argument: $o is not supported.")
      }
    })
  }

  private def makeArgObj[O](ths: Object, m: HashMap[String, OptVal], arg: Class[_], opts: List[Opt]): O = {
    val is = arg.getConstructors()(0)
    val arr =
      (List.empty[AnyRef] /: opts) {
        case (arr_i, o) =>
          m.get(o.name) match {
            case Some(v) => boxed(v) :: arr_i
            case None =>
              o match {
                case OSimple(_, v) => v :: arr_i
                case OStrArg(_, v) => v :: arr_i
                case OIntArg(_, v) => v :: arr_i
              }
          }
      }
    val num = is.getParameterCount
    val aa2 = if (opts.size != num) ths :: arr.reverse else arr.reverse
    assert(aa2.size == num)
    is.newInstance(aa2: _*).asInstanceOf[O]
  }

  private[this] def boxed(v: OptVal): Object = {
    v match {
      case VBool(b) => Boolean.box(b)
      case VStr(b) => b
      case VInt(b) => Int.box(b)
    }
  }

  trait Opts

  trait GPhase[A, B] {
    type OO <: Opts
    lazy val help: String = {
      if (name != null && opts.nonEmpty) {
        val s = StringBuilder.newBuilder
        val desc = if (!description.isEmpty) s": $description" else ""
        s ++= s" * Phase $name$desc\n"

        if (opts.nonEmpty) {
          opts.foreach(o => {
            optDescriptions.get(o.name) match {
              case Some(odesc) =>
                s ++= s"  - ${o.name}: $odesc"
              case None =>
                s ++= s"  - ${o.name}"
            }
            s ++= s" (default: ${o.defToString})\n"
          })
        }

        s.toString()
      } else ""
    }
    lazy val _opts: List[Opt] = getOpts(arg)
    lazy val opts: List[Opt] = _opts
    val arg: Opts = EmptyO
    val name: String
    val description: String = ""
    val optDescriptions: HashMap[String, String] = HashMap.empty[String, String]

    def _init(s: List[String], g: Opts, o: HashMap[String, OptVal]): Try[B] = {
      s |> init |>> _run(s, o, g)
    }

    def _run(gs: List[String], args: HashMap[String, OptVal], g: Opts)(s: A): Try[B] = {
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
    final val phase: GPhase[_, _] = _phase
    lazy val help: String = {
      val s = StringBuilder.newBuilder

      opts.foreach(o => {
        s ++= s"- ${o.name}: "
        optDescriptions.get(o.name) match {
          case Some(desc) => s ++= s"$desc "
          case _ =>
        }
        s ++= s"(default: ${o.defToString})\n"
      })

      s.toString()
    }
    private lazy val opts: List[Opt] = getOpts(arg)
    val optDescriptions: HashMap[String, String] = HashMap.empty[String, String]
    val description: String = ""
    protected val arg: Opts = EmptyO

    final def _init(s: List[String], o: HashMap[String, OptVal]): Try[_] = {
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
    override lazy val help: String = {
      p1.help + p2.help
    }
    override val name: String = null

    override def _run(gs: List[String], m: HashMap[String, OptVal], g: Opts)(s: A): Try[C] = {
      s |> p1._run(gs, m, g) |>> p2._run(gs, m, g)
    }

    override def init(s: List[String]): Try[A] = p1.init(s)
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

  case object UnitCmd extends Cmd(NilPhase)

  object Cmd {
    var map: HashMap[String, Cmd] = HashMap.empty[String, Cmd]
    var default: Cmd = UnitCmd

    def get(name: List[String]): (Cmd, List[String]) =
      if (name.nonEmpty) (map.getOrElse(name.head, default), name.tail) else (default, name)
  }

}
