package controllers

import play.api._
import play.api.mvc._

import play.api.data._
import Forms._
import format.Formats._
import validation._
import validation.Constraints._

import play.api.i18n._

import scala.js._
import scala.jsvalidation.PlayLMS._

case class Fields(a: String, b: Int)

object Application extends Controller {

  val myForm = Form(
    mapping(
      "a" -> of[String],
      "b" -> of[Int].verifying(jsConstraint("constraint.eq5", "error.eq5") { new { def eval(c: JS) = { import c._; (n: Rep[Int]) => n == 5 } } })
    )(Fields.apply)(Fields.unapply)
  )
  
  def index = Action {
    Ok(views.html.index(generateJS(myForm.mapping), myForm))
  }

  def ok = Action { implicit request =>
    myForm.bindFromRequest.fold(
      errors => Ok(views.html.index(generateJS(myForm.mapping), errors)),
      fields => Ok(views.html.ok(fields)))
  }
}
