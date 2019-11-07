import ProbOne.{BestGroupPrice, CabinPrice, Rate}
import org.scalatest.FunSuite

class ProbOneTest extends FunSuite {


  //--------------------------------------------------
  // Given Values
  //
  //--------------------------------------------------
  val givenRates  = List(
    Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior")
  )
  val givenCabinPrices = List(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00)
  )
  val givenExpectedResults = List(
    BestGroupPrice("CA", "M1", 200.00, "Military"),
    BestGroupPrice("CA", "S1", 225.00, "Senior"),
    BestGroupPrice("CB", "M1", 230.00, "Military"),
    BestGroupPrice("CB", "S1", 245.00, "Senior")
  )

  //--------------------------------------------------
  // Test Results are as Expected
  //
  //--------------------------------------------------
  test("ProbOne.getBestGroupPrices.expectedResults"){

    //-- calculate results
    val calculatedResults = ProbOne.getBestGroupPrices(givenRates, givenCabinPrices)

    //-- make sure each expected result is present
    givenExpectedResults.foreach(givenResult => {
      assert(calculatedResults.contains(givenResult))
    })

    //-- verify no extra results are present
    assert(calculatedResults.size == givenExpectedResults.size)
  }

  //--------------------------------------------------
  // Edge Cases Around Empty Data
  //
  //--------------------------------------------------
  test("ProbOne.getBestGroupPrices.emptyParamsDontFail") {
    assert(ProbOne.getBestGroupPrices(Seq(), Seq()) === Seq())
    assert(ProbOne.getBestGroupPrices(givenRates, Seq()) === Seq())
    assert(ProbOne.getBestGroupPrices(Seq(), givenCabinPrices) === Seq())
  }
  test("ProbOne.getBestGroupPrices.noCabinPriceValuesForMilitary") {

    //-- alter given data removing military
    val cabinRatesMinusMilitary = givenCabinPrices.filter(cabinPrice => !cabinPrice.rateCode.startsWith("M"))
    val resultsMinusMilitary = givenExpectedResults.filter(bestGroupPrice => !bestGroupPrice.rateCode.startsWith("M"))

    //-- calculate results
    val calculatedResults = ProbOne.getBestGroupPrices(givenRates, cabinRatesMinusMilitary)

    //-- make sure each expected result is present
    resultsMinusMilitary.foreach(givenResult => {
      assert(calculatedResults.contains(givenResult))
    })

    //-- verify no extra results are present
    assert(calculatedResults.size === resultsMinusMilitary.size)
  }
  test("ProbOne.getBestGroupPrices.noRateValuesForSenior") {

    //-- alter given data removing military
    val ratesMinusSeniors = givenRates.filter(rate => !rate.rateGroup.startsWith("S"))
    val resultsMinusSeniors = givenExpectedResults.filter(bestGroupPrice => !bestGroupPrice.rateGroup.startsWith("S"))

    //-- calculate results
    val calculatedResults = ProbOne.getBestGroupPrices(ratesMinusSeniors, givenCabinPrices)

    //-- make sure each expected result is present
    resultsMinusSeniors.foreach(givenResult => {
      assert(calculatedResults.contains(givenResult))
    })

    //-- verify no extra results are present
    assert(calculatedResults.size === resultsMinusSeniors.size)
  }
}
