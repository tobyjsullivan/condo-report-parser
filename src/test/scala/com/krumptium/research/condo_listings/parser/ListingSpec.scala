package com.krumptium.research.condo_listings.parser

import java.io.File

import com.krumptium.research.condo_listings.parser.types._
import org.joda.time.LocalDate
import org.scalatest._

class ListingSpec extends FlatSpec with Matchers {
  val classLoader = getClass.getClassLoader
  val testListing1 = classLoader.getResource("listing1.html").getFile()
  val listing = Listing(new File(testListing1))
  "listingNumber" should "return a Some with the correct listing number" in {
    listing.listingNumber shouldBe Some("V1118924")
  }

  "postalCode" should "return Some(V6C 3R5) for listing1" in {
    listing.postalCode shouldBe Some("V6C 3R5")
  }

  "state" should "return Some(Active) for listing1" in {
    listing.state shouldBe Some("Active")
  }

  "listPrice" should "return Some(799000) for listing1" in {
    listing.listPrice shouldBe Some(DollarAmount(799000))
  }

  "listDate" should "return Some(2015-04-24) for listing1" in {
    listing.listDate.map(_.toString("yyyy-MM-dd")) shouldBe Some("2015-04-24")
  }

  "daysOnMarket" should "return Some(1) for listing1" in {
    listing.daysOnMarket shouldBe Some(1)
  }

  "localDate" should "return Some(2015-07-31) for listing1" in {
    listing.expiryDate.map(_.toString("yyyy-MM-dd")) shouldBe Some("2015-07-31")
  }

  "subdivisionOrComplex" should "return Some(CALLISTO - COAL HARBOUR) for listing1" in {
    listing.subdivisionOrComplex shouldBe Some("CALLISTO - COAL HARBOUR")
  }

  "previousPrice" should "return None for listing1" in {
    listing.previousPrice shouldBe None
  }

  "originalPrice" should "return Some(799000) for listing1" in {
    listing.originalPrice shouldBe Some(DollarAmount(799000))
  }

  "storiesInBuilding" should "return Some(35) for listing1" in {
    listing.storiesInBuilding shouldBe Some(35)
  }

  "bedrooms" should "return Some(1) for listing1" in {
    listing.bedrooms shouldBe Some(1)
  }

  "bathrooms" should "return Some(1) for listing1" in {
    listing.bathrooms shouldBe Some(1)
  }

  "propertyID" should "return Some(026-130-637) for listing1" in {
    listing.propertyID shouldBe Some("026-130-637")
  }

  "approxYearBuild" should "return Some(Year(2005)) for listing1" in {
    listing.approxYearBuilt shouldBe Some(Year(2005))
  }

  "ageAtListDate" should "return Some(10) for listing1" in {
    listing.ageAtListDate shouldBe Some(10)
  }

  "propertyType" should "return Some(Apartment/Condo) for lisitng1" in {
    listing.propertyType shouldBe Some("Apartment/Condo")
  }

  "zoning" should "return Some(CD-1) for listing1" in {
    listing.zoning shouldBe Some("CD-1")
  }

  "taxes" should "return Some(Taxes(DollarAmount(0), Year(2014))) for lisitng1" in {
    listing.taxes shouldBe Some(Taxes(DollarAmount(0), Year(2014)))
  }

  "hasView" should "return Some(true) for listing1" in {
    listing.hasView shouldBe Some(true)
  }

  "viewDescription" should "return Some(BEAUTIFULLY LANDSCAPED COURTYARD) for listing1" in {
    listing.viewDescription shouldBe Some("BEAUTIFULLY LANDSCAPED COURTYARD")
  }

  "totalParking" should "return Some(2) for listing1" in {
    listing.totalParking shouldBe Some(2)
  }

  "coveredParking" should "return Some(2) for listing 1" in {
    listing.coveredParking shouldBe Some(2)
  }

  "parkingAccess" should "return Some(Front) for listing1" in {
    listing.parkingAccess shouldBe Some("Front")
  }

  "parkingFacilities" should "return Some(List(Garage, Underground)) for listing1" in {
    listing.parkingFacilities shouldBe Some(List("Garage", "Underground"))
  }

  "construction" should "return Some(List(Concrete)) for listing1" in {
    listing.construction shouldBe Some(List("Concrete"))
  }

  "unitsInDevelopment" should "return Some(126) for listing1" in {
    listing.unitsInDevelopment shouldBe Some(126)
  }

  "heatAndFuel" should "return Some(List(Forced Air, Heat Pump)) for listing1" in {
    listing.heatAndFuel shouldBe Some(List("Forced Air", "Heat Pump"))
  }

  "titleToLand" should "return Some(Freehold Strata) for listing1" in {
    listing.titleToLand shouldBe Some("Freehold Strata")
  }

  "outdoorArea" should "return Some(List(None)) for listing1" in {
    listing.outdoorArea shouldBe Some(List("None"))
  }

  "sellersInterest" should "return Some(Registered Owner) for listing1" in {
    listing.sellersInterest shouldBe Some("Registered Owner")
  }

  "bylawRestrictions" should "return two expected strings for listing1" in {
    listing.bylawRestrictions shouldBe Some(List("Pets Allowed w/Rest.", "Rentals Allowed w/Restrictions"))
  }

  "description" should "return Some(string) where the string starts with \"Callisto, Coal\" and ends with \"12:30pm.\" for listing1" in {
    val description = listing.description
    description.isDefined shouldBe true
    description.exists(_.startsWith("Callisto, Coal")) shouldBe true
    description.exists(_.endsWith("12:30pm.")) shouldBe true
  }

  "realtorRemarks" should "return Some(string) where the string starts with \"THE MOST\" and ends with \"ASSESSED VALUE.\" for listing1" in {
    val remarks = listing.realtorRemarks
    remarks.isDefined shouldBe true
    remarks.exists(_.startsWith("THE MOST")) shouldBe true
    remarks.exists(_.endsWith("ASSESSED VALUE.")) shouldBe true
  }

  "commission" should "return Some(3.22%-1ST $100K/1.15% BAL) for listing1" in {
    listing.commission shouldBe Some("3.22%-1ST $100K/1.15% BAL")
  }
}
