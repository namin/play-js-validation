package scala.jsvalidation

import java.io.{File,FileInputStream,FileOutputStream,PrintStream}
import org.scalatest._

import play.api._
import play.api.mvc._

import play.api.data._
import Forms._
import format.Formats._
import validation._
import validation.Constraints._

import scala.js._
import PlayLMS._

class TestPlayLMS extends Suite {
  case class Fields(a: String, b: Int)

  def testSimpleConstraint = {
    val myForm = Form(
      mapping(
        "a" -> of[String],
        "b" -> of[Int].verifying(jsConstraint("constraint.eq5", "error.eq5") { new { def eval(c: JS) = {
          import c._;
          (n: Rep[Int]) => n == 5
        }}})
      )(Fields.apply)(Fields.unapply)
    )
    assertEqualsCheck("simple") {
      generateJS(Messages(_))(myForm.mapping)("#myForm")
    }
  }
  
  val prefix = "test-out/"
  def Messages(msg: String) = msg match {
    case "constraint.eq5" => "5"
    case "error.eq5" => "Must equal 5"
    case _ => msg
  }
  def writeFile(name: String, content: String) {
    val p = new PrintStream(new FileOutputStream(name))
    try { p.print(content) } finally { p.close() }
  }
  def readFile(name: String): String = {
    val buf = new Array[Byte](new File(name).length().toInt)
    val fis = new FileInputStream(name)
    fis.read(buf)
    fis.close()
    new String(buf)
  }
  def assertEqualsCheck(name: String)(content: String): Unit = {
    var failed = true
    try {
      expect(readFile(prefix+name+".check")){content}
      failed = false
    } finally {
      if (failed) writeFile(prefix+name, content)
    }
  }
}
