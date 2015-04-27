package com.krumptium.research.condo_listings.parser.types

case class Room(name: Option[String], floor: Option[String], dimensions: Option[RoomDimension])

case class RoomDimension(length: Inches, width: Inches)

case class Inches(amount: Int) extends AnyVal

object Inches {
  implicit def int2Inches(in: Int): Inches = Inches(in)

  implicit def optInt2OptInches(in: Option[Int]): Option[Inches] = in.map(Inches.apply)
}

case class SquareFeet(amount: Int) extends AnyVal

object SquareFeet {
  implicit def int2SqFeet(in: Int): SquareFeet = SquareFeet(in)

  implicit def optInt2OptSqFeet(in: Option[Int]): Option[SquareFeet] = in.map(SquareFeet.apply)
}
