package org.nlogo.nl2ast

import java.io.{ File, FileOutputStream, PrintStream }
import java.net.URI

import scala.collection.immutable.ListMap
import scala.io.Source

import play.api.libs.json.{ JsArray, Json, JsObject, JsString, JsValue }

import org.nlogo.core.{ AgentKind, Button => CoreButton, Monitor => CoreMonitor, Plot => CorePlot
                      , ProcedureDefinition, Program, Slider => CoreSlider, SourceWrapping }
import org.nlogo.core.model.ModelReader
import org.nlogo.fileformat.NLogoXMLLoader
import org.nlogo.parse.{ CompilerUtilities, FrontEnd }

private type ProcDef = ProcedureDefinition

private case class PrePen(name: String, setupCode: String, updateCode: String)

private sealed trait PreWidget { def index: Int }
private case class PreButton(override val index: Int, code: String, kind: AgentKind) extends PreWidget
private case class PreMonitor(override val index: Int, code: String) extends PreWidget
private case class PreSlider( override val index: Int, minCode: String, maxCode: String
                            , stepCode: String) extends PreWidget
private case class PrePlot( override val index: Int, setupCode: String, updateCode: String
                          , pens: Seq[PrePen]) extends PreWidget

private case class PostPen(name: String, setupDef: ProcDef, updateDef: ProcDef)

private[nl2ast] sealed trait ParsedWidget { def index: Int }
private[nl2ast] case class PostButton(override val index: Int, `def`: ProcDef) extends ParsedWidget
private[nl2ast] case class PostMonitor(override val index: Int, `def`: ProcDef) extends ParsedWidget
private[nl2ast] case class PostSlider( override val index: Int, minDef: ProcDef, maxDef: ProcDef
                                     , stepDef: ProcDef) extends ParsedWidget
private[nl2ast] case class PostPlot( override val index: Int, setupDef: ProcDef, updateDef: ProcDef
                                   , pens: Seq[PostPen]) extends ParsedWidget

private[nl2ast] case class MetaVariables( globals: Seq[String], turtleVars: Seq[String], patchVars: Seq[String]
                                        , linkVars: Seq[String])

private[nl2ast] case class Model( metaVars: MetaVariables, procedures: Seq[ProcedureDefinition]
                                , parsedWidgets: Seq[ParsedWidget])

object NL2AST {

  @main
  def main(args: String*): Unit = {

    val (inURI, outOpt) = parseArgs(args).getOrElse { System.exit(1); ??? }

    val out = inURI |> slurpURI |> parseModel |> AST.buildFrom |> ast2JSON |> cleanupJSON |> serializeJSON

    val outStream = outOpt.fold(System.out)(_ |> (new FileOutputStream(_)) |> (new PrintStream(_)))
    outStream.println(out)
    println("Run success!")

  }

  private def parseArgs(args: Seq[String]): Option[(URI, Option[File])] = {
    if (args.length < 1 || args.length > 2 || args(0) == "--help") {
      System.err.println("""Usage: nl2ast <in> <out>
                           |  - <in>: A URI to a valid '.nlogo' or '.nlogox' file, or file containing plain NetLogo code
                           |  - <out>: (Optional) A file path to write the result out to.  If none supplied, prints to stdout.
                           |""".stripMargin)
      None
    } else {
      val uri = new URI(args(0))
      val out =
        if (args.length > 1)
          Option(new File(args(1)))
        else
          None
      Option((uri, out))
    }
  }

  private def slurpURI(uri: URI): String = {
    val source =
      uri.getScheme match {
        case "file" =>
          Source.fromURI(uri)
        case _ =>
          Source.fromURL(uri.toURL)
      }
    val text = source.mkString
    source.close()
    text
  }

  private def parseModel(text: String): Model = {

    val modelOpt =
      if (text.split("@#\\$#@#\\$#@").length == 12) {                             // .nlogo
        Option(ModelReader.parseModel(text, CompilerUtilities, Map()))
      } else if (text.startsWith("""<?xml version="1.0" encoding="utf-8"?>""")) { // .nlogox
        new NLogoXMLLoader(true, CompilerUtilities, false).readModel(text, "nlogox").toOption
      } else {                                                                    // Plain text
        None
      }

    val (code, widgetGlobals, compilableWidgets) =
      modelOpt.map {
        model =>
          val widgets =
            model.widgets.zipWithIndex.collect {
              case (b: CoreButton , i) if !b.source.isEmpty => PreButton (i, b.source.get, b.buttonKind)
              case (m: CoreMonitor, i) if !m.source.isEmpty => PreMonitor(i, m.source.get)
              case (s: CoreSlider , i)                      => PreSlider (i, s.min, s.max, s.step)
              case (p: CorePlot   , i) =>
                val pens = p.pens.map(pen => PrePen(pen.display, pen.setupCode, pen.updateCode))
                PrePlot(i, p.setupCode, p.updateCode, pens)
            }
          (model.code, model.interfaceGlobals, widgets)
      }.getOrElse((text, Seq(), Seq()))

    val dummyProgram            = Program.empty().copy(interfaceGlobals = widgetGlobals)
    val (procedures, structure) = FrontEnd.frontEnd(code, program = dummyProgram)
    val program                 = structure.program

    enum ProcedureType {
      case Command, Reporter
    }

    import ProcedureType.{ Command, Reporter }

    def parse(code: String, procType: ProcedureType, agentKind: AgentKind = AgentKind.Observer): ProcDef = {
      val isCommand = procType == ProcedureType.Command
      val header    = SourceWrapping.getHeader(agentKind, isCommand)
      val footer    = SourceWrapping.getFooter(isCommand)
      val wrapped   = s"$header$code$footer"
      FrontEnd.frontEnd(wrapped, oldProcedures = structure.procedures, program = program)._1.head
    }

    val parsedWidgets =
      compilableWidgets.map {
        case b: PreButton =>
          PostButton(b.index, parse(b.code, Command, b.kind))
        case m: PreMonitor =>
          PostMonitor(m.index, parse(m.code, Reporter))
        case s: PreSlider =>
          val p = (f: (PreSlider) => String) => parse(f(s), Reporter)
          PostSlider(s.index, p(_.minCode), p(_.maxCode), p(_.stepCode))
        case p: PrePlot =>
          val f = (code: String) => parse(code, Command)
          val pens =
            p.pens.map(
              pen => PostPen(pen.name, f(pen.setupCode), f(pen.updateCode))
            )
          PostPlot(p.index, f(p.setupCode), f(p.updateCode), pens)
      }

    val p =
      program.copy(
        turtleVars = program.turtleVars -- dummyProgram.turtleVars.keys
      ,  patchVars = program. patchVars -- dummyProgram. patchVars.keys
      ,   linkVars = program.  linkVars -- dummyProgram.  linkVars.keys
      )

    def pvLens [T](f: (Program) => ListMap[String, Int]): Seq[String] =
      f(p).keys.toSeq

    val metaVars =
      MetaVariables(p.interfaceGlobals ++ p.userGlobals, pvLens(_.turtleVars), pvLens(_.patchVars), pvLens(_.linkVars))

    Model(metaVars, procedures, parsedWidgets)

  }

  private def ast2JSON(root: Root): JsValue = {

    // Laziness gets involved, because these structures are cyclic, and will otherwise generate
    // "forward reference" errors --Jason B. (9/4/25)
    implicit lazy val reporterProcCallFormat = Json.writes[ReporterProcCall]
    implicit lazy val reporterCallFormat     = Json.writes[ReporterCall]
    implicit lazy val lambdaArgRefFormat     = Json.writes[LambdaArgRef]
    implicit lazy val letRefFormat           = Json.writes[LetRef]
    implicit lazy val procedureArgRefFormat  = Json.writes[ProcedureArgRef]
    implicit lazy val valueFormat            = Json.writes[Value]
    implicit lazy val booleanValFormat       = Json.writes[BooleanVal]
    implicit lazy val numberValFormat        = Json.writes[NumberVal]
    implicit lazy val stringValFormat        = Json.writes[StringVal]
    implicit lazy val listValFormat          = Json.writes[ListVal]
    implicit lazy val nobodyValFormat        = Json.writes[NobodyVal.type]
    implicit lazy val cBlockFormat           = Json.writes[CommandBlock]
    implicit lazy val rBlockFormat           = Json.writes[ReporterBlock]
    implicit lazy val rAppFormat             = Json.writes[ReporterApp]
    implicit lazy val expressionFormat       = Json.writes[Expression]
    implicit lazy val statementFormat        = Json.writes[Statement]
    implicit lazy val varFormat              = Json.writes[Var]
    implicit lazy val singleVarFormat        = Json.writes[SingleVar]
    implicit lazy val multiVarFormat         = Json.writes[MultiVar]
    implicit lazy val letBindingFormat       = Json.writes[LetBinding]
    implicit lazy val multiletBindingFormat  = Json.writes[MultiletBinding]
    implicit lazy val commandAppFormat       = Json.writes[CommandApp]
    implicit lazy val procedureFormat        = Json.writes[Procedure]
    implicit lazy val wProcedureFormat       = Json.writes[WidgetProcedure]
    implicit lazy val astPenFormat           = Json.writes[Pen]
    implicit lazy val astWidgetFormat        = Json.writes[ASTWidget]
    implicit lazy val astButtonFormat        = Json.writes[Button]
    implicit lazy val astMonitorFormat       = Json.writes[Monitor]
    implicit lazy val astSliderFormat        = Json.writes[Slider]
    implicit lazy val astPlotFormat          = Json.writes[Plot]
    implicit lazy val metaVarsFormat         = Json.writes[MetaVariables]
    implicit lazy val rootFormat             = Json.writes[Root]

    Json.toJson(root)

  }

  private def cleanupJSON(json: JsValue): JsValue = {

    def cleanupTypeInner(str: String): String =
      s"${str.head}${str.tail.replaceAll("([A-Z])", "-$1")}".toLowerCase.stripSuffix("-val")

    def cleanupTypeOuter(value: JsValue): JsValue =
      value match {
        case JsString(str) => JsString(cleanupTypeInner(str.stripPrefix("org.nlogo.nl2ast.")))
        case x             => x
      }

    json match {
      case JsArray(values) =>
        JsArray(values.map(cleanupJSON))
      case JsObject(mappings) =>
        val refinedMappings =
          mappings.get("_type").fold(mappings) {
            typ => ListMap("type" -> cleanupTypeOuter(typ)) ++ ListMap(mappings.toSeq*) - "_type"
          }
        JsObject(ListMap(refinedMappings.view.mapValues(cleanupJSON).toSeq*))
      case x =>
        x
    }

  }

  private def serializeJSON(json: JsValue): String =
    Json.stringify(json)

  extension [A](a: A) {
    private infix def |>[B](f: A => B): B = f(a)
  }

}
