package org.virtuslab.stacktraces
package tasty

import scala.quoted.Quotes

import collection.JavaConverters._

class TypesSupport(val qctx: Quotes):
  import qctx.reflect._

  def toLambda(d: DefDef): String =
    s"(${d.paramss(0).params.collect {
      case x: ValDef => asSignature(x.tpt) 
    }.mkString(", ")}) => ${asSignature(d.returnTpt)}"

  def asSignature(tpeTree: Tree): String =
    tpeTree match
      case TypeBoundsTree(low, high) => typeBoundsTreeOfHigherKindedType(low.tpe, high.tpe)
      case tpeTree: TypeTree => inner(tpeTree.tpe)
      case term:  Term => inner(term.tpe)

  // extension (tpe: TypeRepr)
  //   def asSignature: String = inner(tpe)

  extension (t: TypeRepr)
    def isTupleType: Boolean = hackIsTupleType(using qctx)(t)

    def hackIsTupleType(using Quotes)(rtpe: qctx.reflect.TypeRepr): Boolean =
      import dotty.tools.dotc
      given ctx: dotc.core.Contexts.Context = qctx.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
      val tpe = rtpe.asInstanceOf[dotc.core.Types.Type]
      ctx.definitions.isTupleType(tpe)

  private def text(str: String): String = str

  private def texts(str: String): String = text(str)

  private def link(symbol: Symbol): String =
    val suffix = if symbol.isValDef then texts(".type") else ""
    symbol.name + suffix

  private def commas(lists: List[String]) = lists match
    case List(single) => single
    case other => other.reduce((r, e) => r ++ texts(", ") ++ e)

  private def isRepeatedAnnotation(term: Term) =
    term.tpe match
      case t: TypeRef => t.name == "Repeated" && t.qualifier.match
        case ThisType(tref: TypeRef) if tref.name == "internal" => true
        case _ => false
      case _ => false

  private def isRepeated(typeRepr: TypeRepr) =
    typeRepr match
      case t: TypeRef => t.name == "<repeated>" && t.qualifier.match
        case ThisType(tref: TypeRef) if tref.name == "scala" => true
        case _ => false
      case _ => false

  // TODO #23 add support for all types signatures that makes sense
  private def inner(tp: TypeRepr): String =
    def noSupported(name: String): String =
      println(s"WARN: Unsupported type: $name: ${tp.show}")
      text(s"Unsupported[$name]")

    tp match
      case OrType(left, right) => inner(left) ++ texts(" | ") ++ inner(right)
      case AndType(left, right) => inner(left) ++ texts(" & ") ++ inner(right)
      case ByNameType(tpe) => text("=> ") + inner(tpe)
      case ConstantType(constant) =>
        texts(constant.show)
      case ThisType(tpe) => inner(tpe)
      case AnnotatedType(AppliedType(_, Seq(tpe)), annotation) if isRepeatedAnnotation(annotation) =>
        inner(tpe) + text("*")
      case AppliedType(repeatedClass, Seq(tpe)) if isRepeated(repeatedClass) =>
        inner(tpe) + text("*")
      case AnnotatedType(tpe, _) =>
        inner(tpe)
      case tl @ TypeLambda(params, paramBounds, resType) =>
        texts("[") ++ commas(params.zip(paramBounds).map { (name, typ) =>
          val normalizedName = if name.matches("_\\$\\d*") then "_" else name
          texts(normalizedName) ++ inner(typ)
        }) ++ texts("]")
        ++ texts(" =>> ")
        ++ inner(resType)


      case r: Refinement => { //(parent, name, info)
        def getRefinementInformation(t: TypeRepr): List[TypeRepr] = t match {
          case r: Refinement => getRefinementInformation(r.parent) :+ r
          case t => List(t)
        }

        def getParamBounds(t: PolyType): String = commas(
          t.paramNames.zip(t.paramBounds.map(inner(_)))
            .map(b => texts(b(0)) ++ b(1))
        )

        def getParamList(m: MethodType): String =
          texts("(")
          + m.paramNames.zip(m.paramTypes).map{ case (name, tp) => texts(s"$name: ") ++ inner(tp)}
            .reduceLeftOption((acc: String, elem: String) => acc ++ texts(", ") ++ elem).getOrElse(List())
          ++ texts(")")

        def parseRefinedElem(name: String, info: TypeRepr, polyTyped: String = ""): String = ( info match {
          case m: MethodType => {
            val paramList = getParamList(m)
            texts(s"def $name") ++ polyTyped ++ paramList ++ texts(": ") ++ inner(m.resType)
          }
          case t: PolyType => {
            val paramBounds = getParamBounds(t)
            val parsedMethod = parseRefinedElem(name, t.resType)
            if (!paramBounds.isEmpty){
              parseRefinedElem(name, t.resType, texts("[") ++ paramBounds ++ texts("]"))
            } else parseRefinedElem(name, t.resType)
          }
          case ByNameType(tp) => texts(s"def $name: ") ++ inner(tp)
          case t: TypeBounds => texts(s"type $name") ++ inner(t)
          case t: TypeRef => texts(s"val $name: ") ++ inner(t)
          case t: TermRef => texts(s"val $name: ") ++ inner(t)
          case other => noSupported(s"Not supported type in refinement $info")
        } ) ++ texts("; ")

        def parsePolyFunction(info: TypeRepr): String = info match {
          case t: PolyType =>
            val paramBounds = getParamBounds(t)
            val method = t.resType.asInstanceOf[MethodType]
            val paramList = getParamList(method)
            val resType = inner(method.resType)
            texts("[") ++ paramBounds ++ texts("] => ") ++ paramList ++ texts(" => ") ++ resType
          case other => noSupported(s"Not supported type in refinement $info")
        }
        val refinementInfo = getRefinementInformation(r)
        val refinedType = refinementInfo.head
        val refinedElems = refinementInfo.tail.collect{ case r: Refinement => r }.toList
        val prefix: String = if refinedType.typeSymbol != defn.ObjectClass then inner(refinedType) + texts(" ") else ""
        if (refinedType.typeSymbol.fullName == "scala.PolyFunction" && refinedElems.size == 1) {
          parsePolyFunction(refinedElems.head.info)
        } else {
          prefix + texts("{ ") ++ refinedElems.flatMap(e => parseRefinedElem(e.name, e.info)) + texts(" }")
        }
      }
      case t @ AppliedType(tpe, typeList) =>
        import dotty.tools.dotc.util.Chars._
        if !t.typeSymbol.name.forall(isIdentifierPart) && typeList.size == 2 then
          inner(typeList.head)
          ++ texts(" ")
          ++ inner(tpe)
          ++ texts(" ")
          ++ inner(typeList.last)
        else if t.isFunctionType then
          typeList match
            case Nil =>
              ""
            case Seq(rtpe) =>
              text("() => ") + inner(rtpe)
            case Seq(arg, rtpe) =>
              val partOfSignature = arg match
                case byName: ByNameType => texts("(") ++ inner(byName) ++ texts(")")
                case _ => inner(arg)
              partOfSignature ++ texts(" => ") ++ inner(rtpe)
            case args =>
              texts("(") ++ commas(args.init.map(inner)) ++ texts(") => ") ++ inner(args.last)
        else if t.isTupleType then
          typeList match
            case Nil =>
              ""
            case args =>
              texts("(") ++ commas(args.map(inner)) ++ texts(")")
        else inner(tpe) ++ texts("[") ++ commas(typeList.map { t => t match
          case _: TypeBounds => texts("_") ++ inner(t)
          case _ => inner(t)
        }) ++ texts("]")

      case tp @ TypeRef(qual, typeName) =>
        qual match {
          case r: RecursiveThis => texts(s"this.$typeName")
          case _: TypeRepr | _: NoPrefix => link(tp.typeSymbol)
          case null => noSupported(s"TypeRepr: $tp")
        }
        // convertTypeOrBoundsToReference(reflect)(qual) match {
        //     case TypeReference(label, link, xs, _) => TypeReference(typeName, link + "/" + label, xs, true)
        //     case EmptyReference => TypeReference(typeName, "", Nil, true)
        //     case _ if tp.typeSymbol.exists =>
        //     tp.typeSymbol match {
        //         // NOTE: Only TypeRefs can reference ClassDefSymbols
        //         case sym if sym.isClassDef => //Need to be split because these types have their own file
        //         convertTypeOrBoundsToReference(reflect)(qual) match {
        //             case TypeReference(label, link, xs, _) => TypeReference(sym.name, link + "/" + label, xs, true)
        //             case EmptyReference if sym.name == "<root>" | sym.name == "_root_" => EmptyReference
        //             case EmptyReference => TypeReference(sym.name, "", Nil, true)
        //             case _ => throw Exception("Match error in SymRef/TypeOrBounds/ClassDef. This should not happen, please open an issue. " + convertTypeOrBoundsToReference(reflect)(qual))
        //         }

        //         // NOTE: This branch handles packages, which are now TypeRefs
        //         case sym if sym.isTerm || sym.isTypeDef =>
        //         convertTypeOrBoundsToReference(reflect)(qual) match {
        //             case TypeReference(label, link, xs, _) => TypeReference(sym.name, link + "/" + label, xs)
        //             case EmptyReference if sym.name == "<root>" | sym.name == "_root_" => EmptyReference
        //             case EmptyReference => TypeReference(sym.name, "", Nil)
        //             case _ => throw Exception("Match error in SymRef/TypeOrBounds/Other. This should not happen, please open an issue. " + convertTypeOrBoundsToReference(reflect)(qual))
        //         }
        //         case sym => throw Exception("Match error in SymRef. This should not happen, please open an issue. " + sym)
        //     }
        //     case _ =>
        //     throw Exception("Match error in TypeRef. This should not happen, please open an issue. " + convertTypeOrBoundsToReference(reflect)(qual))
        // }
      case tr @ TermRef(qual, typeName) =>
        tr.termSymbol.tree match
          case vd: ValDef => inner(vd.tpt.tpe)
          case _          => link(tr.termSymbol)


        // convertTypeOrBoundsToReference(reflect)(qual) match {
        //     case TypeReference(label, link, xs, _) => TypeReference(typeName + "$", link + "/" + label, xs)
        //     case EmptyReference => TypeReference(typeName, "", Nil)
        //     case _ => throw Exception("Match error in TermRef. This should not happen, please open an issue. " + convertTypeOrBoundsToReference(reflect)(qual))
        // }

      // NOTE: old SymRefs are now either TypeRefs or TermRefs - the logic here needs to be moved into above branches
      // NOTE: _.symbol on *Ref returns its symbol
      // case SymRef(symbol, typeOrBounds) => symbol match {
      // }
      // case _ => throw Exception("No match for type in conversion to Reference. This should not happen, please open an issue. " + tp)
      case TypeBounds(low, hi) =>
        if(low == hi) texts(" = ") ++ inner(low)
        else typeBoundsTreeOfHigherKindedType(low, hi)

      case NoPrefix() => ""

      case MatchType(bond, sc, cases) =>
        val casesTexts = cases.flatMap {
          case MatchCase(from, to) =>
            texts("  case ") ++ inner(from) ++ texts(" => ") ++ inner(to) ++ texts("\n")
          case TypeLambda(_, _, MatchCase(from, to)) =>
            texts("  case ") ++ inner(from) ++ texts(" => ") ++ inner(to) ++ texts("\n")
        }
        inner(sc) + texts(" match {\n") + casesTexts + texts("}")

      case ParamRef(TypeLambda(names, _, _), i) => texts(names.apply(i))

      case ParamRef(m: MethodType, i) => texts(m.paramNames(i))

      case RecursiveType(tp) => inner(tp)

  private def typeBound(t: TypeRepr, low: Boolean): String =
    val ignore = if(low) t.typeSymbol == defn.NothingClass  else t.typeSymbol == defn.AnyClass
    val prefix = text(if low then " >: " else " <: ")
    t match {
      case l: TypeLambda => prefix + texts("(") ++ inner(l) ++ texts(")")
      case p: ParamRef => prefix + inner(p)
      case other if !ignore => prefix + inner(other)
      case _ => ""
    }

  private def typeBoundsTreeOfHigherKindedType(low: TypeRepr, high: TypeRepr): String =
    def regularTypeBounds(low: TypeRepr, high: TypeRepr): String =
      typeBound(low, low = true) + typeBound(high, low = false)
    high.match
      case TypeLambda(params, paramBounds, resType) =>
        if resType.typeSymbol == defn.AnyClass then
          texts("[") ++ commas(params.zip(paramBounds).map { (name, typ) =>
            val normalizedName = if name.matches("_\\$\\d*") then "_" else name
            texts(normalizedName) ++ inner(typ)
          }) ++ texts("]")
        else
          regularTypeBounds(low, high)
      case _ => regularTypeBounds(low, high)
