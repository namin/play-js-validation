package models

case class Comment(
  firstname: String,
  lastname: String,
  company: Option[String],
  email: String,
  phone: Option[String],
  message: String
)
