package gradual.liq

import gradual.db.{CRUD, QExp}
import gradual.pl.Exp

case class EmbedDB(crud: CRUD) extends Exp
case class EmbedPL(e: Exp) extends QExp
