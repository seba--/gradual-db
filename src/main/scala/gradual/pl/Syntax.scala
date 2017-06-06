package gradual.pl

import scala.collection.mutable


// values
sealed trait Val
case object Unit extends Val
case class Obj(cls: Class, fields: mutable.Map[String, Val]) extends Val

// expressions
trait Exp
/** Reference to method parameter or "this". */
case class Ref(x: String) extends Exp
case class New(name: String, fields: Map[String, Exp]) extends Exp
case class Read(receiver: Exp, field: String) extends Exp
case class Write(receiver: Exp, field: String, rhs: Exp) extends Exp
case class Invoke(receiver: Exp, method: String, args: List[Exp]) extends Exp
case class Block(body: List[Exp]) extends Exp
case class If(cond: Exp, thn: Exp, els: Exp) extends Exp
case class While(cond: Exp, body: Exp) extends Exp

// classes
case class Class(
                  name: String,
                  ext: Option[Class],
                  fields: Set[String],
                  methods: Map[String, (List[String], Exp)]) {
  def allfields: Set[String] =
    fields ++ ext.toSet.flatMap((c: Class) => c.allfields)
  def hasField(field: String): Boolean =
    fields.contains(field) || ext.map(_.hasField(field)).getOrElse(false)
  def getMethod(method: String): Option[(List[String], Exp)] =
    methods.get(method).orElse(ext.flatMap(_.getMethod(method)))
}
