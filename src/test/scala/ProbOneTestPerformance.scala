import java.time.{Duration, LocalDate, LocalDateTime}
import java.util.UUID

import ProbOne.{BestGroupPrice, CabinPrice, Rate}
import org.scalatest.FunSuite

import scala.collection.mutable.ListBuffer

class ProbOneTestPerformance extends FunSuite {

  /* -- performance test ... remove to run
  ignore test("ProbOne.getBestGroupPrices.performance") {
    doPerformanceTest(100, 1000)
    1
  }
  */

  private def doPerformanceTest(nbrRates: Int, nbrCabinsPerRate: Int): Unit = {

    //
    //step  1.... make a bunch of random data
    //      i) we make a bunch of rate groups
    //      ii) for each rate group we add a bunch of CabinPrices
    //
    def makeABunchOfRates = (count: Int) =>  {
      val buffer: ListBuffer[Rate] = new ListBuffer[Rate]
      while(buffer.size < count){
        buffer.append(Rate(UUID.randomUUID().toString, UUID.randomUUID().toString))
      }
      buffer.toSeq
    }
    def makeABunchOfCabinPricesForEachRateGroup = (rates: Seq[Rate], count: Int) =>{
      val buffer: ListBuffer[CabinPrice] = new ListBuffer[CabinPrice]
      var completed = 0
      while(completed < count){
        val cabinCode = "C" + completed
        val cabinPricesForRates = rates.map(rate => CabinPrice(cabinCode, rate.rateCode, scala.util.Random.nextDouble()))
        buffer.appendAll(cabinPricesForRates)
        completed = completed + 1
      }
      buffer.toSeq
    }

    val randomRates = makeABunchOfRates(nbrRates)
    val randomCabinPrices = makeABunchOfCabinPricesForEachRateGroup(randomRates, nbrCabinsPerRate)

    //
    //step 2 ... just check sizes of data
    //
    assert(randomRates.size == nbrRates)
    assert(randomCabinPrices.size == (nbrRates * nbrCabinsPerRate))

    //
    //step 3 ... run the caculation
    //
    val start = LocalDateTime.now()
    val calculatedResults = ProbOne.getBestGroupPrices(randomRates, randomCabinPrices)
    val executionTimeMs = Duration.between(start, LocalDateTime.now()).toMillis

    println("[performance] Running BestGroupRates Performance :: duration (ms)=" + executionTimeMs)
    assert(executionTimeMs <= (1000 * 60))
  }
}
