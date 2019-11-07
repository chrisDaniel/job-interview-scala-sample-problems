import ProbOne.{CabinPrice, Rate}
import ProbTwo.Promotion

object Hello extends App {

  //--------------------------------------------------
  // Problem 1
  //
  //--------------------------------------------------
  val allRates  = List(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
  )

  val allCabinPrices = List(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
  )

  println("\nProb 1-1 :: Best Cabin Prices for Each Group")
  ProbOne.getBestGroupPrices(allRates, allCabinPrices).foreach(x => println("\t" + x))


  //--------------------------------------------------
  // Problem 2
  //
  //--------------------------------------------------
  val allPromotions = List(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
  )
  println("\nProb 2-1 :: All Promotion Combinations")
  ProbTwo.allCombinablePromotions(allPromotions).foreach(x => println("\t" + x.promotionCodes))

  println("\nProb 2-2 :: Promotion Combinations for promo code :: P1")
  ProbTwo.combinablePromotions("P1", allPromotions).foreach(x => println("\t" + x.promotionCodes))

  println("\nProb 2-3 :: Promotion Combinations for promo code :: P3")
  ProbTwo.combinablePromotions("P3", allPromotions).foreach(x => println("\t" + x.promotionCodes))
}
