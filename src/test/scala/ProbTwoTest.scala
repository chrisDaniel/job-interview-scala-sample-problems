
import ProbTwo.{Promotion, PromotionCombo}
import org.scalatest.FunSuite

class ProbTwoTest extends FunSuite {

  //--------------------------------------------------
  // Given Values
  //
  //--------------------------------------------------
  val givenPromotions = Seq(
    Promotion("P1", Seq("P3")),
    Promotion("P2", Seq("P4", "P5")),
    Promotion("P3", Seq("P1")),
    Promotion("P4", Seq("P2")),
    Promotion("P5", Seq("P2"))
  )

  val givenExpectedResults_allCombined = Seq(
    PromotionCombo(Seq("P1", "P2")),
    PromotionCombo(Seq("P1", "P4", "P5")),
    PromotionCombo(Seq("P2", "P3")),
    PromotionCombo(Seq("P3", "P4", "P5"))
  )
  val givenExpectedResults_P1 = Seq(
    PromotionCombo(Seq("P1", "P2")),
    PromotionCombo(Seq("P1", "P4", "P5"))
  )
  val givenExpectedResults_P3 = Seq(
    PromotionCombo(Seq("P2", "P3")),
    PromotionCombo(Seq("P3", "P4", "P5"))
  )

  //--------------------------------------------------
  // Test Results are as Expected
  //
  //--------------------------------------------------
  test("ProbTwo.allCombinablePromotions.expectedResults"){

    //-- calculate results
    val calculatedResults = ProbTwo.allCombinablePromotions(givenPromotions)

    //-- make sure each expected result is present
    givenExpectedResults_allCombined.foreach(givenResult => {
      assert(calculatedResults.contains(givenResult))
    })

    //-- verify no extra results are present
    assert(calculatedResults.size == givenExpectedResults_allCombined.size)
  }
  test("ProbTwo.combinablePromotions(P1).expectedResults"){

    //-- calculate results
    val calculatedResults = ProbTwo.combinablePromotions("P1", givenPromotions)

    //-- make sure each expected result is present
    givenExpectedResults_P1.foreach(givenResult => {
      assert(calculatedResults.contains(givenResult))
    })

    //-- verify no extra results are present
    assert(calculatedResults.size == givenExpectedResults_P1.size)
  }
  test("ProbTwo.combinablePromotions(P3).expectedResults"){

    //-- calculate results
    val calculatedResults = ProbTwo.combinablePromotions("P3", givenPromotions)

    //-- make sure each expected result is present
    givenExpectedResults_P3.foreach(givenResult => {
      assert(calculatedResults.contains(givenResult))
    })

    //-- verify no extra results are present
    assert(calculatedResults.size == givenExpectedResults_P3.size)
  }

  //--------------------------------------------------
  // Edge Cases Around Empty Data
  //
  //--------------------------------------------------
  test("ProbTwo.allCombinablePromotions.emptyParamsDontFail") {
    assert(ProbTwo.allCombinablePromotions(Seq()) === Seq())
  }
  test("ProbTwo.combinablePromotions.emptyParamsDontFail") {
    assert(ProbTwo.combinablePromotions("", Seq()) === Seq())
    assert(ProbTwo.combinablePromotions("", givenPromotions) === Seq())
    assert(ProbTwo.combinablePromotions("P1", Seq()) === Seq())
  }
}
