package org.nlogo.nl2ast

import java.lang.{ Boolean => JBoolean, Double => JDouble }

import org.nlogo.core.{ CommandBlock => NLCBlock, Expression => NLExpr, LogoList, Nobody, ProcedureDefinition
                      , ReporterApp => NLRApp, ReporterBlock => NLRBlock, Statement => NLStatement }

import org.nlogo.core.prim.{ _abstractlet => AbstractLet, _callreport => CallReport, _const => Const
                           , _let => Let, _multilet => Multilet }

private[nl2ast] case class Command(name: String)
private[nl2ast] case class Reporter(name: String)

private[nl2ast] sealed trait Expression
private[nl2ast] case class CommandBlock(statements: Seq[Statement]) extends Expression
private[nl2ast] case class ReporterBlock(reporterApp: ReporterApp) extends Expression

private[nl2ast] sealed trait ReporterApp extends Expression
private[nl2ast] case class ReporterProcCall(reporter: Reporter, args: Seq[Expression]) extends ReporterApp
private[nl2ast] case class ReporterCall(reporter: Reporter, args: Seq[Expression]) extends ReporterApp
private[nl2ast] case class Constant(value: Value) extends ReporterApp

private[nl2ast] sealed trait Value
private[nl2ast] case class BooleanVal(value: Boolean) extends Value
private[nl2ast] case class NumberVal(value: Double) extends Value
private[nl2ast] case class StringVal(value: String) extends Value
private[nl2ast] case class ListVal(items: Seq[Value]) extends Value
private[nl2ast] case object NobodyVal extends Value

private[nl2ast] sealed trait Statement
private[nl2ast] case class LetBinding(varName: String, value: Expression) extends Statement
private[nl2ast] case class MultiletBinding(vars: Seq[Var], value: Expression) extends Statement
private[nl2ast] case class CommandApp(command: Command, args: Seq[Expression]) extends Statement

private[nl2ast] sealed trait Var
private[nl2ast] case class SingleVar(name: String) extends Var
private[nl2ast] case class MultiVar(vars: Seq[Var]) extends Var

private[nl2ast] case class Procedure( name: String, args: Seq[String], returnType: String, agentClass: String
                                    , statements: Seq[Statement])
private[nl2ast] case class Root(procedures: Seq[Procedure], metaVars: MetaVariables)

object AST {

  def buildFrom(model: Model): Root = {
    Root(model.procedures.map(convertProcedure), model.metaVars)
  }

  private def convertProcedure(procDef: ProcedureDefinition): Procedure = {

    val statements = processStatements(procDef.statements.stmts)

    val proc = procDef.procedure

    Procedure( proc.name
             , proc.args
             , if (proc.isReporter) "wildcard" else "unit"
             , proc.agentClassString
             , statements
             )

  }

  // Essentially: When we have the code `to my-procedure let [x y] [1 2] end`, the NL parser emits the AST:
  // `my-procedure [(_multilet [x y] [1 2]) (_let x x) (_let y y)]`.  Why?  I don't entirely know.  But
  // I don't think it's entirely appropriate to have these redundant `let`s here, so this function's
  // purpose is to remove them. --Jason B. (9/11/25)
  private def processStatements(statements: Seq[NLStatement]): Seq[Statement] =
    statements.foldLeft((Seq[Statement](), Set[String]())) {
      case ((acc, names), statement) =>
        val (converted, additionalNames) = convertStatement(statement)
        val combinedNames                = names ++ additionalNames
        converted match {
          case l: LetBinding if combinedNames.contains(l.varName) => (acc     , combinedNames - l.varName)
          case x                                                  => (acc :+ x, combinedNames)
        }
    }._1

  private def convertStatement(statement: NLStatement): (Statement, Set[String]) = {
    val args = statement.args.map(convertExpression)
    statement.command match {
      case l: Let =>
        (LetBinding(makeLetVar(l).name, args.head), Set())
      case ml: Multilet =>
        val vars  = ml.lets.map(convertMultiVar)
        val names = vars.foldLeft(Set[String]()) { case (acc, v) => extractVarNames(v, acc) }
        (MultiletBinding(vars, args.head), names)
      case _      =>
        (CommandApp(Command(statement.command.displayName), args), Set())
    }
  }

  private def extractVarNames(vars: Var, acc: Set[String]): Set[String] =
    vars match {
      case SingleVar(name) => acc + name
      case MultiVar(vars)  => vars.foldLeft(acc) { case (bcc, x) => extractVarNames(x, bcc) }
    }

  private def convertMultiVar(ml: AbstractLet): Var =
    ml match {
      case l: Let =>
        makeLetVar(l)
      case ml: Multilet =>
        MultiVar(ml.lets.map(convertMultiVar))
    }

  private def makeLetVar(l: Let): SingleVar = {
    val varName = l.let.map(_.name).getOrElse(throw new Exception("Impossible unnamed `let` binding"))
    SingleVar(varName)
  }

  private def convertExpression(expr: NLExpr): Expression = {
    expr match {
      case cb: NLCBlock => CommandBlock(processStatements(cb.statements.stmts))
      case rb: NLRBlock => ReporterBlock(convertReporterApp(rb.app))
      case ra: NLRApp   => convertReporterApp(ra)
    }
  }

  private def convertReporterApp(rApp: NLRApp): ReporterApp =
    rApp.reporter match {
      case const: Const =>
        val value = convertLiteral(const.value)
        Constant(value)
      case _: CallReport =>
        ReporterProcCall(Reporter(rApp.reporter.displayName), rApp.args.map(convertExpression))
      case _ =>
        ReporterCall(Reporter(rApp.reporter.displayName), rApp.args.map(convertExpression))
    }

  private def convertLiteral(literal: AnyRef): Value =
    literal match {
      case b: JBoolean => BooleanVal(b)
      case x: JDouble  => NumberVal(x)
      case s: String   => StringVal(s)
      case l: LogoList => ListVal(l.toVector.map(convertLiteral))
      case Nobody      => NobodyVal
      case x =>
        throw new Exception(s"We don't know how to convert this type of reporter yet: ${x.getClass.getName} | $x")
    }

}
