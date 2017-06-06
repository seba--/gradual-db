package gradual.db

import scala.collection.immutable.ListMap

case class DB(name: String)

sealed trait DBType
case object DBInteger extends DBType
case class DBVarChar(n: Int) extends DBType

case class Result(rows: List[List[DBVal]])

sealed trait DBVal
sealed trait DBValNumeric extends DBVal {
  def <(that: DBValNumeric): Boolean
  def >(that: DBValNumeric): Boolean
}
case class DBIntegerVal(n: Int) extends DBValNumeric {
  override def <(that: DBValNumeric): Boolean = that match {
    case DBIntegerVal(n2) => n < n2
  }
  override def >(that: DBValNumeric): Boolean = that match {
    case DBIntegerVal(n2) => n > n2
  }
}
case class DBVarCharVal(s: String) extends DBVal

sealed trait CRUD
case class Create(db: DB, table: String, schema: ListMap[String, DBType]) extends CRUD
case class Read(db: DB, query: Query) extends CRUD
case class Update(db: DB, update: UpdateQuery) extends CRUD
case class Delete(db: DB, table: String) extends CRUD

case class Query(select: Select, from: From, where: Option[QExp])
sealed trait Select
case object All extends Select
case class Som(cols: List[String]) extends Select
case class From(tables: List[String], vars: List[String]) {
  assert(tables.nonEmpty)
  assert(tables.size == vars.size)
}

// TODO
sealed trait UpdateQuery


trait QExp
case class ColumnSelection(tvar: String, col: String) extends QExp
case class Compare(op: CompareOp, e1: QExp, e2: QExp) extends QExp
case class And(e1: QExp, e2: QExp) extends QExp
case class Or(e1: QExp, e2: QExp) extends QExp

sealed trait CompareOp
case object EqOp extends CompareOp
case object GtOp extends CompareOp
case object LtOp extends CompareOp