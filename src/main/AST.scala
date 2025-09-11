package org.nlogo.nl2ast

import java.lang.{ Boolean => JBoolean, Double => JDouble }

import org.nlogo.core.{ CommandBlock => NLCBlock, Expression => NLExpr, LogoList, Nobody, ProcedureDefinition
                      , ReporterApp => NLRApp, ReporterBlock => NLRBlock, Statement => NLStatement }

import org.nlogo.core.prim.{ _callreport => CallReport, _const => Const }

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

private[nl2ast] case class Statement(command: Command, args: Seq[Expression])
private[nl2ast] case class Procedure( name: String, args: Seq[String], returnType: String, agentClass: String
                                    , statements: Seq[Statement])
private[nl2ast] case class Root(procedures: Seq[Procedure], metaVars: MetaVariables)

object AST {

  def buildFrom(model: Model): Root = {
    Root(model.procedures.map(convertProcedure), model.metaVars)
  }

  private def convertProcedure(procDef: ProcedureDefinition): Procedure = {

    val statements = procDef.statements.stmts.map(convertStatement)

    val proc = procDef.procedure

    Procedure( proc.name
             , proc.args
             , if (proc.isReporter) "wildcard" else "unit"
             , proc.agentClassString
             , statements
             )

  }

  private def convertStatement(statement: NLStatement): Statement = {
    Statement(Command(statement.command.displayName), statement.args.map(convertExpression))
  }

  private def convertExpression(expr: NLExpr): Expression = {
    expr match {
      case cb: NLCBlock => CommandBlock(cb.statements.stmts.map(convertStatement))
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
