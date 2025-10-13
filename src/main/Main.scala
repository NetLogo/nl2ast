package org.nlogo.nl2ast

import java.io.{ File, FileOutputStream, PrintStream }
import java.net.URI

import scala.io.Source

extension [A](a: A) {
  private[nl2ast] infix def |>[B](f: A => B): B = f(a)
}

object NL2AST {

  @main
  def main(args: String*): Unit = {

    val (inURI, outOpt) = parseArgs(args).getOrElse { System.exit(1); ??? }

    val out = inURI |> slurpURI |> Parser.apply |> AST.buildFrom |> Serializer.apply

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

}
