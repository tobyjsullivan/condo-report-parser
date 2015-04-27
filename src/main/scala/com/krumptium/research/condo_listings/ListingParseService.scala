package com.krumptium.research.condo_listings

import java.io.File

import com.krumptium.research.condo_listings.parser.Listing

object ListingParseService extends App {
  // Read each page
  // TODO
  val testFileName = "condo-research_V1118960_8277662109958046797.html"
  val file = new File(testFileName)
  if (!file.exists())
    throw new Exception("Wrong file!")

  // Parse components out of page
  val listing = Listing(file)

  // Generate JSON object
  // TODO

  // Append object to output file
  // TODO

  // Upload final output file
  // TODO
}
