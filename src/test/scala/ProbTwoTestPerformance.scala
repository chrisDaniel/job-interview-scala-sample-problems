import java.time.{Duration, LocalDate, LocalDateTime}
import java.util.UUID

import ProbOne.{BestGroupPrice, CabinPrice, Rate}
import ProbTwo.Promotion
import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer
import scala.util.Random

class ProbTwoTestPerformance extends FunSuite {

  /* -- performance test ... remove to run
  test("ProbTwo.allCombinablePromotions.performance") {
    doPerformanceTest(10)
  }
  */

  def doPerformanceTest(nbrPromotions: Int) = {

    //
    //step  1.... make a bunch of random data
    //      i) we make a bunch of rate groups
    //      ii) for each rate group we add a bunch of CabinPrices
    //
    def makeABunchOfPromotions_notCombinableList = () => {
      val buffer: ListBuffer[String] = new ListBuffer[String]
      val nbrForBlacklist = Random.nextInt(nbrPromotions)
      while(buffer.size < nbrForBlacklist){
        val promoCodeForBlacklist = "P-" + Random.nextInt(nbrPromotions)
        buffer.append(promoCodeForBlacklist)
      }
      buffer.toSeq
    }
    def makeABunchOfPromotions = () =>  {
      val buffer: ListBuffer[Promotion] = new ListBuffer[Promotion]
      while(buffer.size < nbrPromotions){
        val promoCode = "P-" + buffer.size
        buffer.append(Promotion(promoCode, makeABunchOfPromotions_notCombinableList()))
      }
      buffer.toSeq
    }


    val randomPromos = makeABunchOfPromotions()

    //
    //step 2 ... just check sizes of data
    //
    assert(randomPromos.size == nbrPromotions)

    //
    //step 3 ... run the caculation
    //
    val start = LocalDateTime.now()
    val calculatedResults = ProbTwo.allCombinablePromotions(randomPromos)
    val executionTimeMs = Duration.between(start, LocalDateTime.now()).toMillis

    println("[performance] Running AllCombinablePromos Performance :: duration (ms)=" + executionTimeMs)
    assert(executionTimeMs <= (1000 * 10))
  }
}