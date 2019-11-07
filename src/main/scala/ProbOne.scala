object ProbOne {

  //----------------------------------------------------
  // Problem 1 Model
  //
  //----------------------------------------------------
  case class Rate(rateCode: String, rateGroup: String)

  case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)

  case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)


  //----------------------------------------------------
  // Problem 1
  //
  //----------------------------------------------------
  /** Retrieve the Seq[BestGroupPrice] given a Seq[Rates] and Seq[CabinPrice]
    *
    *  This result set covers all combinations of [CabinPrice.cabinCode x Rate.rateGroup]
    *  Logic
    *        a) get unique cabin codes
    *        b) for each cabinCode ... get the BestGroupPrice ... for each Rate.rateGroup
    *
    *  @param rates Seq of Rate
    *  @param prices Seq of CabinPrice
    *  @return Sequence of BestGroupPrice
    */
  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {

    //
    // a... get unique cabinCodes
    //
    val uniqueCabinCodes: Set[String] = prices.map(price => price.cabinCode).toSet

    //
    // b... process each unique cabinCode
    //      bi)   select CabinPrices where CabinPrice.cabinCode == cabinCode
    //      bii)  flatten on  call to helper function 1 => Seq[BestGroupPrice]
    //
    uniqueCabinCodes.flatMap(cabinCode => {
        val allPricesForCabinCode = prices.filter(price => cabinCode.equalsIgnoreCase(price.cabinCode))
        getBestGroupPricesForCabinCode(cabinCode, rates, allPricesForCabinCode)
      })
      .toSeq
  }

  /** Helper function 1 for getBestGroupPrices
    *
    * For a cabinCode ... calculate the BestGroupPrice for each unique rateGroup
    *
    * @param cabinCode CabinCode.cabinCode
    * @param rates Seq of Rate to consider
    * @param cabinPrices All cabinPrices to consider
    * @return Seq of BestGroupPrice one for each unique Rate.rateGroup
  */
  def getBestGroupPricesForCabinCode(cabinCode: String, rates: Seq[Rate], cabinPrices: Seq[CabinPrice]): Seq[BestGroupPrice] = {

    //
    // a...
    //    a1 - lookup map of Rate.rateCode -> Rate.rateGroup
    //    a2 - a function ... rateGroup => Seq[CabinPrice]
    //
    val rateCodeToRateGroupMap = rates.map(i => i.rateCode -> i.rateGroup).toMap

    def getPricesForRateGroup(myRateGroup: String) : Seq[CabinPrice] = {

      cabinPrices.filter(price => {
        if(!rateCodeToRateGroupMap.contains(price.rateCode)){
          false
        }
        else {
          val priceRateGroup = rateCodeToRateGroupMap(price.rateCode)
          myRateGroup equalsIgnoreCase priceRateGroup
        }
      })
    }

    //
    // b....
    //     b1 - get unique rate groups
    //     b2 - for each group ... find lowest CabinPrice
    //     b3 - for lowest ... if present map to a BestGroupPrice
    //     b4 - careful ... we can get nulls here ... so filter em out
    //
    rates.map(i => i.rateGroup)
         .toSet
         .map((rateGroupString: String) => {
           val pricesForRateGroup = getPricesForRateGroup(rateGroupString)
           val lowestPriceO = getLowestCabinPriceFromList(pricesForRateGroup)
           lowestPriceO.map(l => BestGroupPrice(l.cabinCode, l.rateCode, l.price, rateGroupString)).orNull
         })
         .filter(bestGroupPrice => null != bestGroupPrice)
      .toSeq
  }

  /** Helper function 2 for getBestGroupPrices
    *
    * For a Seq[CabinPrice] finds the lowest CabinPrice.
    *
    * @param prices Seq of CabinPrice to evaluate
    * @return CabinPrice with the lowest CabinPrice.price
    */
  private def getLowestCabinPriceFromList(prices: Seq[CabinPrice])  = {
    if(null == prices || prices.isEmpty) {
      Option.empty
    }
    else {
      val returnSmallest = (a: CabinPrice, b: CabinPrice) => if (a.price < b.price) a else b
      val smallest = prices.foldLeft(prices(0))(returnSmallest)
      Option.apply(smallest)
    }
  }
}
