package com.krumptium.research.condo_listings.parser

import java.io.File

import com.krumptium.research.condo_listings.parser.types._
import org.joda.time.format._
import org.joda.time.{LocalDateTime, DateTime, LocalDate}
import org.jsoup.Jsoup
import org.jsoup.nodes._
import org.jsoup.select.Elements

import scala.io.Source
import scala.collection.JavaConverters._
import scala.util.Try

case class Listing(htmlFile: File) {
  private val dateParseFormat = DateTimeFormat.forPattern("dd-MMM-yy")

  private val document = {
    val html = Source.fromFile(htmlFile).mkString
    Jsoup.parse(html)
  }

  implicit private def elementsToListElement(elements: Elements): List[Element] =
    elements.iterator().asScala.toList

  private def parseBooleanOption(boolString: String): Option[Boolean] = boolString.trim match {
    case "Y" => Some(true)
    case "N" => Some(false)
    case "Yes" => Some(true)
    case "No" => Some(false)
    case _ => Try(java.lang.Boolean.parseBoolean(boolString)).toOption
  }

  private def parseIntOption(intString: String): Option[Int] =
    Try(Integer.parseInt(intString.filterNot("$,".toSet))).toOption

  private def parseDateOption(dateString: String): Option[LocalDate] =
    Try(dateParseFormat.parseLocalDate(dateString)).toOption

  private def splitList(listOfPhrases: String): List[String] = {
    val regexSeparators = "[;/,]"

    // Temporarily replace any confusing words
    val confusingWords = Seq("w/")

    val foundWordIdxs = for (
      idx <- 0 until confusingWords.size;
      word = confusingWords(idx) if listOfPhrases.contains(word)
    ) yield idx

    val stringWithConfusingWordsReplaced = foundWordIdxs.foldLeft(listOfPhrases) { (acc, idx) =>
      val confusingWord = confusingWords(idx)
      val temporaryWord = s"{{%$idx%}}"
      acc.replace(confusingWord, temporaryWord)
    }

    // Do the actual splitting
    val phrases = stringWithConfusingWordsReplaced.split(regexSeparators).toList.map(_.trim)

    // Restore confusing words
    phrases.map { phrase =>
      foundWordIdxs.foldLeft(phrase) { (acc, idx) =>
        val confusingWord = confusingWords(idx)
        acc.replace(s"{{%$idx%}}", confusingWord)
      }
    }
  }

  private val spanIdxMap: Map[Symbol, Int] = Map(
    'listingNumber -> 1,
    'address -> 2,
    'state -> 3
  )

  lazy private val allTheSpans: List[Element] = document.select("body > span")

  private case class SpanElement(element: Element) {
    private val style: String = element.attr("style")

    private def parseStyleInt(attrName: String): Option[Int] = {
      val regex = s"$attrName:(\\d+)px".r
      val matching = regex
        .findFirstMatchIn(style)
        .flatMap(_.subgroups.headOption)

      matching.flatMap(parseIntOption)
    }

    val left: Option[Int] = parseStyleInt("left")

    val top: Option[Int] = parseStyleInt("top")

    val width: Option[Int] = parseStyleInt("width")

    val height: Option[Int] = parseStyleInt("height")

    val right: Option[Int] = left.flatMap(l => width.map(_ + l))

    val bottom: Option[Int] = top.flatMap(t => height.map(_ + t))

    val isBold: Boolean = style.contains("font:bold")

    val text: String = element.text()

    def isToLeftOf(other: SpanElement): Boolean = {
      // A span, other, is to the right if other.left > this.left (allowing overlap)
      // AND other.bottom > this.top
      // AND other.top < this.bottom
      val oRes = for (
        myLeft <- this.left;
        theirLeft <- other.left;
        myTop <- this.top;
        theirTop <- other.top;
        myBottom <- this.bottom;
        theirBottom <- other.bottom
      ) yield {
        theirLeft > myLeft &&
        theirBottom > myTop &&
        theirTop < myBottom
      }

      oRes.getOrElse(false)
    }
  }

  private case object Layout {
    // Get a list of spans containing category titles or values (denoted by their height of 13px).
    val propertySpans: List[SpanElement] =
      allTheSpans.map(SpanElement.apply)
        .filter(_.height.exists(h => h >= 13 && h <= 23))

    def findSpanToRightOf(span: SpanElement): Option[SpanElement] = {
      val allSpansToRight = propertySpans.filter(span.isToLeftOf)

      // Filter anything that isn't at least close vertically
      val spansWithinAFewYPixles = allSpansToRight.filter(cur => Math.abs(cur.top.getOrElse(0) - span.top.getOrElse(0)) < 3)

      val orderedByX = spansWithinAFewYPixles.sortBy(_.left)

      orderedByX.headOption
    }

    def findSpanElement(element: Element): Option[SpanElement] =
      propertySpans.find(_.element == element)
  }

  private def getSpanTextByIndex(idx: Int): Option[String] = {
    val oSpan = if (allTheSpans.length > idx)
      Some(allTheSpans(idx))
    else
      None

    oSpan.map(_.text())
  }

  private def getSpanTextByTitle(title: String): Option[String] = {
    val titleText = s"$title:"
    val selector = s"span:contains($titleText)"
    val matches = document.select(selector)
    // Contains selector returns all elements which contain text. We need to find exact match
    val oTitleElement = matches
      .find(_.text().trim == titleText)

    val oTitleSpanElement = oTitleElement.flatMap(Layout.findSpanElement)

    val oValueElement = oTitleSpanElement.flatMap(Layout.findSpanToRightOf).filter(_.isBold)

    oValueElement.map(_.text)
  }

  lazy val listingNumber: Option[String] = {
    val oFullTitle = getSpanTextByIndex(spanIdxMap('listingNumber))

    oFullTitle.flatMap { title =>
      if (title.startsWith("MLS# "))
        Some(title.substring(5))
      else
        None
    }
  }

  lazy val postalCode: Option[String] = {
    val oFullAddress = getSpanTextByIndex(spanIdxMap('address))

    oFullAddress.flatMap { fullAddress =>
      val splitPos = fullAddress.lastIndexOf(',')

      if (splitPos == -1 || splitPos == (fullAddress.length - 1))
        None
      else
        Some(fullAddress.substring(splitPos + 1).trim)
    }
  }

  lazy val state: Option[String] = getSpanTextByIndex(spanIdxMap('state))

  lazy val listPrice: Option[DollarAmount] = {
    // Get the span following the title
    val oPriceText = getSpanTextByTitle("List Price")

    oPriceText.flatMap(parseIntOption).map(DollarAmount.apply)
  }

  lazy val listDate: Option[LocalDate] =
    getSpanTextByTitle("List Date").flatMap(parseDateOption)

  lazy val daysOnMarket: Option[Int] =
    getSpanTextByTitle("Days on Mkt").flatMap(parseIntOption)

  lazy val expiryDate: Option[LocalDate] =
    getSpanTextByTitle("Expiry Date").flatMap(parseDateOption)

  lazy val subdivisionOrComplex: Option[String] =
    getSpanTextByTitle("Subdiv/Complex")

  lazy val previousPrice: Option[DollarAmount] =
    getSpanTextByTitle("Previous Price").flatMap(parseIntOption)

  lazy val originalPrice: Option[DollarAmount] =
    getSpanTextByTitle("Original Price").flatMap(parseIntOption)

  lazy val storiesInBuilding: Option[Int] =
    getSpanTextByTitle("Stories in Bldg").flatMap(parseIntOption)

  lazy val bedrooms: Option[Int] =
    getSpanTextByTitle("Bedrooms").flatMap(parseIntOption)

  lazy val bathrooms: Option[Int] =
    getSpanTextByTitle("Bathrooms").flatMap(parseIntOption)

  lazy val approvalRequired: Option[Boolean] = ???

  lazy val propertyID: Option[String] =
    getSpanTextByTitle("PID")

  lazy val approxYearBuilt: Option[Year] =
    getSpanTextByTitle("Approx Yr Blt").flatMap(parseIntOption)

  lazy val ageAtListDate: Option[Int] = getSpanTextByTitle("Age at List Date").flatMap(parseIntOption)

  lazy val propertyType: Option[String] = getSpanTextByTitle("Type")

  lazy val zoning: Option[String] = getSpanTextByTitle("Zoning")

  lazy val taxes: Option[Taxes] = {
    val oTaxString = getSpanTextByTitle("Taxes")
    println("Tax string: "+oTaxString.getOrElse(""))

    val oAmount: Option[DollarAmount] = oTaxString.flatMap{ taxString =>
      val regexAmount = "\\$(\\d+)".r
      val oAmountString = regexAmount.findFirstMatchIn(taxString)
      oAmountString.flatMap(_.subgroups.headOption)
    } flatMap parseIntOption

    val oYear: Option[Year] = oTaxString.flatMap{ taxString =>
      val regexYear = "\\((\\d+)\\)".r
      regexYear
        .findFirstMatchIn(taxString)
        .flatMap(_.subgroups.headOption)
    } flatMap parseIntOption

    for (
      amount <- oAmount;
      year <- oYear
    ) yield Taxes(amount, year)
  }

  lazy val hasView: Option[Boolean] = {
    val oViewString = getSpanTextByTitle("View")

    oViewString.flatMap { viewString =>
      val boolString = viewString.takeWhile(_ != '-').trim
      parseBooleanOption(boolString)
    }
  }

  lazy val viewDescription: Option[String] = {
    // For some stupid reason, this field follows "Flood Plain" but it's not necessarily the immediate next field...
    val oViewString = getSpanTextByTitle("View")

    oViewString.flatMap { viewString =>
      val splitterIdx = viewString.indexOf('-')
      if (splitterIdx == -1 || splitterIdx == viewString.length - 1)
        None
      else {
        Some(viewString.substring(splitterIdx + 1).trim)
      }
    }
  }

  lazy val styleOfHome: Option[String] = ???

  lazy val totalParking: Option[Int] = getSpanTextByTitle("Total Parking").flatMap(parseIntOption)

  lazy val coveredParking: Option[Int] = getSpanTextByTitle("Covered Parking").flatMap(parseIntOption)

  lazy val parkingAccess: Option[String] = getSpanTextByTitle("Parking Access")

  lazy val parkingFacilities: Option[List[String]] = getSpanTextByTitle("Parking Facilities").map(splitList)

  lazy val construction: Option[List[String]] = getSpanTextByTitle("Construction").map(splitList)

  lazy val foundation: Option[List[String]] = ???

  lazy val exterior: Option[List[String]] = ???

  lazy val typeOfRoof: Option[String] = ???

  lazy val distanceToPublicTransit: Option[String] = ???

  lazy val distanceToSchoolOrBus: Option[String] = ???

  lazy val renovations: Option[List[String]] = ???

  lazy val unitsInDevelopment: Option[Int] = getSpanTextByTitle("Units in Development").flatMap(parseIntOption)

  lazy val totalUnitsInStrata: Option[Int] = ???

  lazy val flooring: Option[List[String]] = ???

  lazy val locker: Option[Boolean] = ???

  lazy val waterSupply: Option[String] = ???

  lazy val heatAndFuel: Option[List[String]] = getSpanTextByTitle("Heat/Fuel").map(splitList)

  lazy val titleToLand: Option[String] = getSpanTextByTitle("Title to Land")

  lazy val numFireplaces: Option[Int] = ???

  lazy val fireplaceFuel: Option[List[String]] = ???

  lazy val sellersInterest: Option[String] = getSpanTextByTitle("Seller's Interest")

  lazy val outdoorArea: Option[List[String]] = getSpanTextByTitle("Outdoor Area").map(splitList)

  lazy val propertyDisclosure: Option[Boolean] = ???

  lazy val propertyDisclosureDescription: Option[String] = ???

  lazy val managementCompany: Option[Contact] = ???

  lazy val bylawRestrictions: Option[List[String]] = getSpanTextByTitle("Bylaw Restrictions").map(splitList)

  lazy val maintenanceFee: Option[DollarAmount] = ???

  lazy val maintenanceChargeIncludes: Option[List[String]] = ???

  lazy val legal: Option[String] = ???

  lazy val amenities: Option[List[String]] = ???

  lazy val featuresInclude: Option[List[String]] = ???

  lazy val siteInfluences: Option[List[String]] = ???

  lazy val servicesConnected: Option[List[String]] = ???

  lazy val rooms: Option[List[Room]] = ???

  lazy val numRooms: Option[Int] = ???

  lazy val grandTotalFloorArea: Option[SquareFeet] = ???

  lazy val listingBrokers: Option[List[Contact]] = ???

  lazy val listingSalesReps: Option[List[Contact]] = ???

  lazy val appointments: Option[String] = ???

  lazy val appointmentContact: Option[Contact] = ???

  lazy val occupancy: Option[String] = ???

  lazy val owner: Option[String] = ???

  lazy val commission: Option[String] = getSpanTextByTitle("Commission")

  lazy val realtorRemarks: Option[String] = {
    val spanRealtorRemarksTitle = document.select("span:contains(Realtor Remarks:)").find(_.text() == "Realtor Remarks:")

    spanRealtorRemarksTitle.flatMap(span => Try(span.previousElementSibling().text()).toOption)
  }

  lazy val description: Option[String] = {
    val spanRealtorRemarksTitle = document.select("span:contains(Realtor Remarks:)").find(_.text() == "Realtor Remarks:")

    spanRealtorRemarksTitle.flatMap(span => Try(span.nextElementSibling().text()).toOption)
  }

  lazy val reportGenerated: Option[LocalDateTime] = ???

  lazy val address: Option[String] = ???

  lazy val neighbourhood: Option[String] = ???
}
