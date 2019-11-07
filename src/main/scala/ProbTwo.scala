import scala.annotation.tailrec

object ProbTwo {

  //----------------------------------------------------
  // Problem 2 Model
  //
  //----------------------------------------------------
  case class Promotion(code: String, notCombinableWith: Seq[String])
  case class PromotionCombo(promotionCodes: Seq[String])

  /** Helper function - Create a new PromotionCombo by appending a new Promotion
    *
    * @param combo PromotionCombo to append to
    * @param promo Promotion to append
    * @return new PromotionCombo with Promotion added
    */
  private def comboPlusPromotion  (combo: PromotionCombo, promo: Promotion) = {
    PromotionCombo(Seq.concat(combo.promotionCodes, List(promo.code)))
  }

  //----------------------------------------------------
  // Problem 2-1
  //
  //----------------------------------------------------
  /** Calculate all `best` and `legal` PromotionCombos give a Seq of PromotionCode
    *
    *  part 1 - Get all possible legal combinations
    *          (i) a function to determine if a combination is legal
    *          (ii) a function to calculate all legal combinations
    *
    *  part 2 - reduce all possible combinations to only the `best`
    *           `best` is illustrated by the below example
    *               (a) is a subset of (a, b)
    *               so (a) is not a `best` combination
    *               while (a, b) may be a `best`
    *
    *  @param allPromotions Seq of Promotions to consider
    *  @return Seq of (best and legal) PromotionCombos
    */
  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo]  = {

    //
    // part 1 (i) - Legal Combination logic
    //
    val blacklistLookupMap = allPromotions.map(promotion => promotion.code -> promotion.notCombinableWith.toSet).toMap

    def canAddToCombo (combo: PromotionCombo, promoToAdd: Promotion)= {

      def isCombinationIlLegal(code1: String, code2: String): Boolean = {
        if (blacklistLookupMap(code1).contains(code2)) true
        else if (blacklistLookupMap(code2).contains(code1)) true
        else false
      }

      combo.promotionCodes.count(code => isCombinationIlLegal(code, promoToAdd.code)) <= 0
    }

    //
    // part 1 (ii) - All Possible Combinations Function
    //
    @tailrec
    def allPossibleCombosAcc (acc: Seq[PromotionCombo], remaining: Seq[Promotion]) : Seq[PromotionCombo] = {

      if(remaining.size <= 0) {
        acc
      }
      else {
        val headPromo = remaining.head
        val accConcatenateHead = acc
          .filter(promtionCombo => canAddToCombo(promtionCombo, headPromo))
          .map(promotionCombo => comboPlusPromotion(promotionCombo, headPromo))

        val updatedAccumulator = Seq.concat(acc, accConcatenateHead, List(PromotionCombo(List(headPromo.code))))

        allPossibleCombosAcc(updatedAccumulator, remaining.drop(1))
      }
    }
    val allPossibleCombos = allPossibleCombosAcc(List(), allPromotions)


    //
    // part 2.... Reduce All Permutations to a `best` list
    //
    def doesComboAsupersetComboB(comboA: PromotionCombo, comboB: PromotionCombo) ={
      comboB.promotionCodes.count(code => !comboA.promotionCodes.contains(code)) <= 0
    }
    def isComboABestCombo (comboToCheck: PromotionCombo)={
      !allPossibleCombos.exists(thatCombo => {
        if(thatCombo.equals(comboToCheck))
          false
        else
          doesComboAsupersetComboB(thatCombo, comboToCheck)
      })
    }

    //
    // final calculation
    //
    allPossibleCombos.filter(permutation => isComboABestCombo(permutation))
  }

  //----------------------------------------------------
  // Problem 2-2
  //
  //----------------------------------------------------
  /**  Calculate all `best` and `legal` PromotionCombos for a specific promotionCode
    *
    *  @param promotionCode String value of Code to filter on
    *  @param allPromotions Seq of Promotions to consider
    *  @return Seq of (best and legal) PromotionCombos
  */
  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {

    allCombinablePromotions(allPromotions)
        .filter(combo => combo.promotionCodes.contains(promotionCode))
  }
}
