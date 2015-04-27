package com.krumptium.research.condo_listings.parser.types

case class DollarAmount(amount: Int) extends AnyVal

object DollarAmount {
  implicit def int2DollarAmount(in: Int): DollarAmount = DollarAmount(in)

  implicit def optInt2OptDollarAmount(in: Option[Int]): Option[DollarAmount] = in.map(DollarAmount.apply)
}
