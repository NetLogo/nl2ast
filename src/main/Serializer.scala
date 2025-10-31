package org.nlogo.nl2ast

import scala.collection.immutable.ListMap

import play.api.libs.json.{ JsArray, Json, JsObject, JsString, JsValue }

object Serializer {

  def apply(ast: Root): String =
    ast |> ast2JSON |> cleanupJSON |> Json.stringify

  private def ast2JSON(root: Root): JsValue = {

    // Laziness gets involved, because these structures are cyclic, and will otherwise generate
    // "forward reference" errors --Jason B. (9/4/25)
    implicit lazy val reporterProcCallFormat = Json.writes[ReporterProcCall]
    implicit lazy val reporterCallFormat     = Json.writes[ReporterCall]
    implicit lazy val lambdaArgRefFormat     = Json.writes[LambdaArgRef]
    implicit lazy val globalVarFormat        = Json.writes[GlobalVar]
    implicit lazy val linkVarFormat          = Json.writes[LinkVar]
    implicit lazy val patchVarFormat         = Json.writes[PatchVar]
    implicit lazy val turtleVarFormat        = Json.writes[TurtleVar]
    implicit lazy val turtleOrLinkVarFormat  = Json.writes[TurtleOrLinkVar]
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
    implicit lazy val commandProcCallFormat  = Json.writes[CommandProcCall]
    implicit lazy val commandCallFormat      = Json.writes[CommandCall]
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

}
