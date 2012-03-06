package scala.jsvalidation

import play.api._
import play.api.mvc._

import play.api.data._
import Forms._
import format.Formats._
import validation._
import validation.Constraints._

//import play.api.i18n._

import scala.js._

object PlayLMS {
  abstract class JSConstraint[-T:Manifest](name: String, errorName: String, f: T => Boolean) extends Constraint[T](Some(name), Seq[Any]())(o => if (f(o)) Valid else Invalid(ValidationError(errorName))) {
    val jsExp: JSExp
    val fExp: jsExp.Rep[T] => jsExp.Rep[Boolean]

    def jsName = name.replace(".", "$")
    def validatorRule = jsName + " : true"
    def validatorCode: String = {
      val writer = new java.io.StringWriter
      jsExp.reset
      val codegen = new JSGen { val IR: jsExp.type = jsExp }
      codegen.emitSource(fExp, "", new java.io.PrintWriter(writer))
      val fCode = writer.toString
      val res =
        "jQuery.validator.addMethod(\"" + jsName + "\", function(value, element, params) {\n" +
      "return this.optional(element) || (" + fCode + ")(value);\n" +
      "}, jQuery.format(\"" + errorName + "\"));\n" // Messages(errorName)
       res
     }
  }

  object inScala extends JSInScala
  object inJS extends JSExp


  def jsConstraint[T:Manifest](name: String, errorName: String)(prog: { def eval(c: JS): c.Rep[T] => c.Rep[Boolean] }) = {
    val f = prog.eval(inScala)
    val fExpArg = prog.eval(inJS)
    new JSConstraint[T](name, errorName, f) {
      override val jsExp: inJS.type = inJS
      override val fExp = fExpArg
    }
  }

    def generateJS[T](top: Mapping[T]) = { id: String =>
    var validators : Map[String, String] = Map()
    var res = ""
    res += "rules : {\n"
    
    for (m <- top.mappings; if !m.key.isEmpty() && !m.constraints.isEmpty) {
      res += m.key + ": {\n"
      res += "required : true,\n"
      for (c <- m.constraints) {
        c match {
          case jsC : JSConstraint[_] => {
            res += jsC.validatorRule + ",\n"
            validators += ((jsC.jsName, jsC.validatorCode))
          }
          case _ => ()
        }
      }
      res += "},\n"
    }

    res += "}\n"

    res = "$(document).ready(function(){\n$(\"" + id + "\").validate({" + res + "})\n})"

    res = validators.values.mkString("\n") + res

    res
  }
}
