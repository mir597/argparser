package com.argparser.ArgParser

sealed abstract class Opt(val name: String) {
  def defToString: String
}

final case class OSimple(override val name: String, default: java.lang.Boolean) extends Opt(name) {
  override def defToString: String = default.toString
}

final case class OStrArg(override val name: String, default: String) extends Opt(name) {
  override def defToString: String = default.toString
}

final case class OIntArg(override val name: String, default: Integer) extends Opt(name) {
  override def defToString: String = default.toString
}

sealed trait OptVal

final case class VBool(v: Boolean) extends OptVal

final case class VStr(v: String) extends OptVal

final case class VInt(v: Int) extends OptVal
