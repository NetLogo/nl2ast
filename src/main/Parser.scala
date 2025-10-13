package org.nlogo.nl2ast

import scala.collection.immutable.ListMap

import org.nlogo.core.{ AgentKind, Button => CoreButton, Monitor => CoreMonitor, Plot => CorePlot
                      , ProcedureDefinition, Program, Slider => CoreSlider, SourceWrapping, Widget => CoreWidget }
import org.nlogo.core.FrontEndInterface.ProceduresMap
import org.nlogo.core.model.ModelReader
import org.nlogo.fileformat.NLogoXMLLoader
import org.nlogo.parse.{ CompilerUtilities, FrontEnd }

private type ProcDef = ProcedureDefinition

private case class ParsedPen(name: String, setupDef: ProcDef, updateDef: ProcDef)

private[nl2ast] sealed trait ParsedWidget { def index: Int }
private[nl2ast] case class ParsedButton(override val index: Int, `def`: ProcDef) extends ParsedWidget
private[nl2ast] case class ParsedMonitor(override val index: Int, `def`: ProcDef) extends ParsedWidget
private[nl2ast] case class ParsedSlider( override val index: Int, minDef: ProcDef, maxDef: ProcDef
                                       , stepDef: ProcDef) extends ParsedWidget
private[nl2ast] case class ParsedPlot( override val index: Int, setupDef: ProcDef, updateDef: ProcDef
                                     , pens: Seq[ParsedPen]) extends ParsedWidget

private[nl2ast] case class MetaVariables( globals: Seq[String], turtleVars: Seq[String], patchVars: Seq[String]
                                        , linkVars: Seq[String])

private[nl2ast] case class Model( metaVars: MetaVariables, procedures: Seq[ProcedureDefinition]
                                , parsedWidgets: Seq[ParsedWidget])

object Parser {

  def apply(text: String): Model = {

    val modelOpt =
      if (text.split("@#\\$#@#\\$#@").length == 12) {                             // .nlogo
        Option(ModelReader.parseModel(text, CompilerUtilities, Map()))
      } else if (text.startsWith("""<?xml version="1.0" encoding="utf-8"?>""")) { // .nlogox
        new NLogoXMLLoader(true, CompilerUtilities, false).readModel(text, "nlogox").toOption
      } else {                                                                    // Plain text
        None
      }

    val (code, widgetGlobals)   = modelOpt.map { model => (model.code, model.interfaceGlobals) }.getOrElse((text, Seq()))
    val dummyProgram            = Program.empty().copy(interfaceGlobals = widgetGlobals)
    val (procedures, structure) = FrontEnd.frontEnd(code, program = dummyProgram)
    val program                 = structure.program

    val metaVars = extractMetaVars(program, dummyProgram)
    val widgets  = modelOpt.map(_.widgets).fold(Seq())(extractWidgets(structure.procedures, program))

    Model(metaVars, procedures, widgets)

  }

  private def extractMetaVars(program: Program, baseProgram: Program): MetaVariables = {

    val p =
      program.copy(
        turtleVars = program.turtleVars -- baseProgram.turtleVars.keys
      ,  patchVars = program. patchVars -- baseProgram. patchVars.keys
      ,   linkVars = program.  linkVars -- baseProgram.  linkVars.keys
      )

    def pvLens [T](f: (Program) => ListMap[String, Int]): Seq[String] =
      f(p).keys.toSeq

    MetaVariables(p.interfaceGlobals ++ p.userGlobals, pvLens(_.turtleVars), pvLens(_.patchVars), pvLens(_.linkVars))

  }

  private def extractWidgets(procedures: ProceduresMap, program: Program)(widgets: Seq[CoreWidget]): Seq[ParsedWidget] = {

    enum ProcedureType {
      case Command, Reporter
    }

    import ProcedureType.{ Command, Reporter }

    def parse(code: String, procType: ProcedureType, agentKind: AgentKind = AgentKind.Observer): ProcDef = {
      val isCommand = procType == Command
      val header    = SourceWrapping.getHeader(agentKind, isCommand)
      val footer    = SourceWrapping.getFooter(isCommand)
      val wrapped   = s"$header$code$footer"
      FrontEnd.frontEnd(wrapped, oldProcedures = procedures, program = program)._1.head
    }

    widgets.zipWithIndex.collect {
      case (b: CoreButton , i) if !b.source.isEmpty => ParsedButton (i, parse(b.source.get, Command, b.buttonKind))
      case (m: CoreMonitor, i) if !m.source.isEmpty => ParsedMonitor(i, parse(m.source.get, Reporter))
      case (s: CoreSlider , i)                      =>
        val p = (f: (CoreSlider) => String) => parse(f(s), Reporter)
        ParsedSlider(i, p(_.min), p(_.max), p(_.step))
      case (p: CorePlot   , i) =>
        val f    = (code: String) => parse(code, Command)
        val pens = p.pens.map(pen => ParsedPen(pen.display, f(pen.setupCode), f(pen.updateCode)))
        ParsedPlot(i, f(p.setupCode), f(p.updateCode), pens)
    }

  }

}
