package com.krumptium.research.condo_listings.parser.types

case class Year(year: Int) extends AnyVal

object Year {
  implicit def int2Year(in: Int): Year = Year(in)

  implicit def optInt2OptYear(in: Option[Int]): Option[Year] = in.map(Year.apply)
}
