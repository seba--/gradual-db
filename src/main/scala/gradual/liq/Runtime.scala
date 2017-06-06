package gradual.liq

import gradual.db.{DB, DBVal, QExp, Result}
import gradual.db.Runtime.WhereVal
import gradual.{db, pl}
import gradual.pl.{Class, Exp, Obj, Val}
import pl.Runtime._

import scala.collection.mutable

object Runtime {
  val oNil = Obj(Class("Nil", None, Set(), Map()), mutable.Map())
}
import Runtime._

trait Runtime extends pl.Runtime with db.Runtime {
  override def eval(exp: Exp, env: Env): Val = exp match {
    case EmbedDB(crud) => eval(crud) match {
      case Result(Nil) => oNil
      case Result(data) => ???
    }
    case _ => super.eval(exp, env)
  }

  override def evalWhere(
                 db: DB,
                 e: QExp,
                 vars: Map[String, Int],
                 tuple: List[Array[DBVal]],
                 colIndexes: List[Map[String, Int]],
                 tables: List[String]): WhereVal = e match {
    case EmbedPL(e) => ???
    case _ => super.evalWhere(db, e, vars, tuple, colIndexes, tables)
  }
}
