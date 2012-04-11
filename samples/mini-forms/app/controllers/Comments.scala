package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._

import views._

import models._

import play.api.i18n._
import scala.js._
import scala.jsvalidation.PlayLMS._

object Comments extends Controller {
  val commentForm: Form[Comment] = Form(
    mapping(
      "firstname" -> nonEmptyText,
      "lastname" -> nonEmptyText,
      "company" -> optional(text),
      "email" -> email,
      "phone" -> optional(text verifying jsPattern("""[0-9.+]+""", "constraint.phone", "A valid phone number is required")),
      "message" -> nonEmptyText.verifying(jsConstraint("constraint.nice_message", "error.nice_message") { new { def eval(c: JS) = { import c._; (msg: Rep[String]) => {
        val words = msg.split(" ")
        def countWords(regex: String) = words.filter(regex.r.test(_)).length
        val hateCount = countWords("[Hh]ate|[Ss]uck")
        val loveCount = countWords("[Ll]ove|[Rr]ock")
        hateCount < loveCount
     }}}})
    )(Comment.apply)(Comment.unapply)
  )
  
  lazy val js = generateJS(Messages(_), twitterBootstrap = true, playDefaults = true)(commentForm.mapping)

  /**
   * Display an empty form.
   */
  def form = Action {
    Ok(html.comment.form(js, commentForm));
  }
  
  /**
   * Display a form pre-filled with an existing Comment.
   */
  def editForm = Action {
    val existingComment = Comment(
      "Nada", "Amin", Some("EPFL"), "namin@alum.mit.edu", Some("41.79.275.15.35"), "I love Scala."
    )
    Ok(html.comment.form(js, commentForm.fill(existingComment)))
  }
  
  /**
   * Handle form submission.
   */
  def submit = Action { implicit request =>
    commentForm.bindFromRequest.fold(
      errors => BadRequest(html.comment.form(js, errors)),
      comment => Ok(html.comment.summary(comment))
    )
  }
  
}
