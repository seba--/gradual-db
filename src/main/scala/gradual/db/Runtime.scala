package gradual.db

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object Runtime {
  type Store = mutable.Map[String, Table]
  case class Table(types: Map[String, DBType], colIndex: Map[String, Int], rows: mutable.ListBuffer[Array[DBVal]])

  sealed trait WhereVal {
    def isTrue: Boolean = false
  }
  case class BoolWhereVal(b: Boolean) extends WhereVal {
    override def isTrue: Boolean = b
  }
  case class DBValWhereVal(v: DBVal) extends WhereVal

  sealed trait DBTypeError extends Throwable
  case class NoSuchDB(db: DB) extends DBTypeError
  case class NoSuchTable(db: DB, table: String) extends DBTypeError
  case class NoSuchColumn(db: DB, table: String, col: String) extends DBTypeError
  case class TableAlreadyExists(db: DB, table: String) extends DBTypeError
  case class UndefinedTableVar(v: String) extends DBTypeError
  case class WrongType(v: WhereVal, expected: String) extends DBTypeError

  def newInstance(definedDBs: Map[String, Store]): Runtime = new Runtime {
    override val dbs = definedDBs
  }
}
import Runtime._

trait Runtime {
  val dbs: Map[String, Store]

  def eval(crud: CRUD): Result = crud match {
    case Create(db, table, schema) =>
      val store = connect(db)
      if (store.contains(table))
        throw TableAlreadyExists(db, table)
      val colIndex = schema.map(_._1).zipWithIndex.toMap
      store += table -> Table(schema, colIndex, mutable.ListBuffer())
      Result(List())

    case Read(db, query) =>
      val store = connect(db)
      evalQuery(db, query, store)

    case Update(db, update) =>
      // TODO
      ???

    case Delete(db, table) =>
      val store = connect(db)
      if (!store.contains(table))
        throw NoSuchTable(db, table)
      store -= table
      Result(List())
  }

  def connect(db: DB): Store = db match {
    case DB(name) => dbs.get(name).getOrElse(throw NoSuchDB(db))
  }


  def evalQuery(db: DB, q: Query, store: Store): Result = {
    val tables = q.from.tables.map { table =>
      store.getOrElse(table, throw NoSuchTable(db, table)).rows.toList
    }
    val colIndexes = q.from.tables.map { table =>
        store(table).colIndex
    }
    val vars = q.from.vars.zipWithIndex.toMap
    val tuples = crossProduct(tables)
    val result =
      for (tuple <- tuples
            if q.where.isEmpty || evalWhere(db, q.where.get, vars, tuple, colIndexes, q.from.tables).isTrue)
        yield evalSelect(db, q.select, vars, tuple, colIndexes)
    Result(result)
  }


  def evalWhere(
                 db: DB,
                 e: QExp,
                 vars: Map[String, Int],
                 tuple: List[Array[DBVal]],
                 colIndexes: List[Map[String, Int]],
                 tables: List[String]): WhereVal = e match {
    case ColumnSelection(tvar, col) =>
      val ix = vars.getOrElse(tvar, throw UndefinedTableVar(tvar))
      val colIndex = colIndexes(ix)
      val colIx = colIndex.getOrElse(col, throw NoSuchColumn(db, tables(ix), col))
      DBValWhereVal(tuple(ix)(colIx))

    case Compare(op, e1, e2) =>
      val v1 = evalWhere(db, e1, vars, tuple, colIndexes, tables)
      val v2 = evalWhere(db, e2, vars, tuple, colIndexes, tables)
      evalCompare(op, v1, v2)

    case And(e1, e2) =>
      val v1 = evalWhere(db, e1, vars, tuple, colIndexes, tables)
      if (!v1.isTrue)
        v1
      else
        evalWhere(db, e2, vars, tuple, colIndexes, tables)

    case Or(e1, e2) =>
      val v1 = evalWhere(db, e1, vars, tuple, colIndexes, tables)
      if (v1.isTrue)
        v1
      else
        evalWhere(db, e2, vars, tuple, colIndexes, tables)
  }

  def evalCompare(op: CompareOp, v1: WhereVal, v2: WhereVal): WhereVal = op match {
    case EqOp => BoolWhereVal(v1 == v2)
    case GtOp =>
      if (!v1.isInstanceOf[DBValNumeric])
        throw WrongType(v1, "Numeric")
      if (!v2.isInstanceOf[DBValNumeric])
        throw WrongType(v2, "Numeric")
      BoolWhereVal(v1.asInstanceOf[DBValNumeric] > v2.asInstanceOf[DBValNumeric])
    case LtOp =>
      if (!v1.isInstanceOf[DBValNumeric])
        throw WrongType(v1, "Numeric")
      if (!v2.isInstanceOf[DBValNumeric])
        throw WrongType(v2, "Numeric")
      BoolWhereVal(v1.asInstanceOf[DBValNumeric] < v2.asInstanceOf[DBValNumeric])
  }

  def evalSelect(
                  db: DB,
                  select: Select,
                  vars: Map[String, Int],
                  tuple: List[Array[DBVal]],
                  colIndexes: List[Map[String, Int]]): List[DBVal] = select match {
    case All => tuple.flatten
    case Som(cols) =>
      val result = ListBuffer[DBVal]()
      for (col <- cols) {
        (0 until tuple.size).find(i =>
          colIndexes(i).get(col) match {
            case Some(ix) =>
              result += tuple(i)(ix)
              true
            case None =>
              false
          }
        )
      }
      result.toList
  }

  def crossProduct[Row](tables: List[List[Row]]): List[List[Row]] = tables match {
    case Nil => throw new MatchError()
    case table::Nil => table.map(List(_))
    case table::rest =>
      val restProduct = crossProduct(rest)
      for (row <- table;
           restRow <- restProduct)
        yield row +: restRow
  }



}

