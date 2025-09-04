package org.nlogo.nl2ast

import org.nlogo.core.{ CommandBlock => NLCBlock, Expression => NLExpr, ProcedureDefinition, ReporterApp => NLRApp, ReporterBlock => NLRBlock, Statement => NLStatement }

private[nl2ast] case class Command(name: String)
private[nl2ast] case class Reporter(name: String)

private[nl2ast] sealed trait Expression
private[nl2ast] case class CommandBlock(statements: Seq[Statement]) extends Expression
private[nl2ast] case class ReporterBlock(reporterApp: ReporterApp) extends Expression
private[nl2ast] case class ReporterApp(reporter: Reporter, args: Seq[Expression]) extends Expression

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

    Procedure( proc.displayName
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

  private def convertReporterApp(rApp: NLRApp): ReporterApp = {
    ReporterApp(Reporter(rApp.reporter.displayName), rApp.args.map(convertExpression))
  }

}
