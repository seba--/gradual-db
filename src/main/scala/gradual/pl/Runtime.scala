package gradual.pl

import scala.collection.mutable

object Runtime {
  type Env = Map[String, Val]
  type CT = Map[String, Class]

  trait RuntimeTypeError extends Throwable
  case class WrongFieldsFailure(cls: Class, fields: Map[String, Exp]) extends RuntimeTypeError
  case class WrongParamsFailure(cls: Class, method: String, params: List[String], args: List[Exp]) extends RuntimeTypeError
  case class NoSuchField(receiver: Val, field: String) extends RuntimeTypeError
  case class NoSuchMethod(receiver: Val, method: String) extends RuntimeTypeError
  case class WrongType(v: Val, expected: String) extends RuntimeTypeError

  def newInstance(definedCT: CT): Runtime = new Runtime {
    override val ct: CT = definedCT
  }
}
import Runtime._

trait Runtime {
  val ct: CT

  def eval(exp: Exp, env: Env): Val = exp match {
    case Ref(x) => env(x)

    case New(name, fields) =>
      val cls = ct(name)
      if (cls.allfields != fields.keys.toSet)
        throw WrongFieldsFailure(cls, fields)
      val fieldVals = mutable.Map() ++ fields.mapValues(eval(_, env))
      Obj(cls, fieldVals)

    case Read(receiver, field) =>
      eval(receiver, env) match {
        case Unit => throw NoSuchField(Unit, field)
        case obj@Obj(cls, fields) =>
          if (!cls.hasField(field))
            throw NoSuchField(obj, field)
          fields(field)
      }

    case Write(receiver, field, rhs) =>
      eval(receiver, env) match {
        case Unit => throw NoSuchField(Unit, field)
        case obj@Obj(cls, fields) =>
          if (!cls.hasField(field))
            throw NoSuchField(obj, field)
          val newval = eval(rhs, env)
          fields += field -> newval
          Unit
      }

    case Invoke(receiver, method, args) =>
      eval(receiver, env) match {
        case Unit => throw NoSuchMethod(Unit, method)
        case obj@Obj(cls, fields) =>
          cls.getMethod(method) match {
            case None => throw NoSuchMethod(obj, method)
            case Some((params, body)) =>
              if (params.size != args.size)
                throw WrongParamsFailure(cls, method, params, args)
              val argVals = args.map(eval(_, env))
              eval(body, Map("this" -> obj) ++ (params zip argVals))
          }
      }

    case Block(body) =>
      body.foldLeft[Val](Unit)((_, e) => eval(e, env))

    case If(cond, thn, els) =>
      eval(cond, env) match {
        case Unit => throw WrongType(Unit, "True or False")
        case obj@Obj(cls, fields) =>
          cls.name match {
            case "True" => eval(thn, env)
            case "False" => eval(els, env)
            case _ => throw WrongType(obj, "True or False")
          }
      }

    case loop@While(cond, body) =>
      eval(cond, env) match {
        case Unit => throw WrongType(Unit, "True or False")
        case obj@Obj(cls, fields) =>
          cls.name match {
            case "True" => eval(body, env); eval(loop, env)
            case "False" => Unit
            case _ => throw WrongType(obj, "True or False")
          }
      }
  }
}
