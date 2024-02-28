package dotty.tools.pc

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.NameKinds.ContextBoundParamName

case class Params(labels: Seq[String], kind: Params.Kind)

object Params:
  enum Kind:
    case TypeParameter, Normal, Implicit, Using, Synthetic

  def paramsKind(syms: List[SingleDenotation])(using Context): Params.Kind =
    val filteredSyms = syms.filter(!_.name.toString.startsWith(ContextBoundParamName.separator))
    if syms.isEmpty then Kind.Normal
    else if filteredSyms.isEmpty then Kind.Synthetic
    else if filteredSyms.head.isType then Kind.TypeParameter
    else if filteredSyms.head.symbol.is(Given) then Kind.Using
    else if filteredSyms.head.symbol.is(Implicit) then Kind.Implicit
    else Kind.Normal
