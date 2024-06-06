package promotion_combos

case class Promotion(code: String, notCombinableWith: Seq[String])

case class PromotionCombo(promotionCodes: Seq[String]) {
  override def equals(that: Any) =
    that match {
      case combo : PromotionCombo => (this.promotionCodes.toSet &~ combo.promotionCodes.toSet).isEmpty
      case _ => false
    }
  
  override def hashCode() =
    this.promotionCodes.foldLeft(0x0)(_.hashCode ^ _.hashCode)
}



// One major consideration we need to make here is what are
// the guarantees of our input dataset `allPromotions`?
//
// Consider the following input set edge case:
//    Promotion("P1", Seq.empty)
//    Promotion("P2", Seq("P1"))
//
// In an ideal world the input data set is "clean" and in such
// a scenario, instead of Seq.empty it should also specify that 
// it is not combinable with P2.
//
// Our ideal implementation of this function is going to vary
// based on whether or not such an invariant holds:
//
// 1. Assuming the invariant holds, we can recurse through the
// set of all unique promotions knowing only what remains to be 
// be potentially appended to the end of a PromotionCombo 
// sequence, and what the current Promotion is.
//
// 2. If the invariant does not hold, we need to also have 
// "backwards" knowledge of what has been appended so far.
// This increases space requirements; but we can limit it by
// recursing this parameter as a View (read-only reference).
//
// As such an invariant on the input data was not specified,
// we should play it safe and assume that our input dataset
// is inconsistent; especially if it's coming from a third
// party API provider that we can't audit.
def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
  // Recursively find combinable chains of promotions by starting from an initial promotion
  // and branching into compatible (combinable) remaining promotions.
  def findPromotionChains(
    promotionCodesInChain: Set[String],
    promotionToAdd: Promotion,
    remainingPromotions: Set[Promotion]
  ): Set[PromotionCombo] = {
    def isForwardsCompatible(p : Promotion) : Boolean =
      !promotionToAdd.notCombinableWith.contains(p.code)

    val isBackwardsCompatible = 
      (promotionCodesInChain & promotionToAdd.notCombinableWith.toSet).isEmpty

    lazy val remainingCompatiblePromotions = 
      remainingPromotions.filter(isForwardsCompatible(_))

    // Base case #1: End the chain and don't append the current promotion if it isn't backwards compatible.
    if !isBackwardsCompatible then
      return Set(PromotionCombo(promotionCodesInChain.toSeq))

    // Base case #2: End the chain and append the current promotion if there are no more remaining promotions to traverse.
    if remainingCompatiblePromotions.isEmpty then
      return Set(PromotionCombo((promotionCodesInChain + promotionToAdd.code).toSeq))

    // Recursive case: Explore the remaining combinable promotions as individual chains and flatten
    // the resulting chains (sets).
    remainingCompatiblePromotions.flatMap(p => 
      findPromotionChains(
        promotionCodesInChain + promotionToAdd.code, 
        p, 
        remainingCompatiblePromotions.excl(p)
      )
    )
  }

  allPromotions.foldLeft((Set.empty : Set[PromotionCombo], allPromotions.toSet)){ case ((accum, possibilities), p) =>
    val filteredPossibilities = possibilities.excl(p)
    (accum.union(findPromotionChains(Set.empty, p, filteredPossibilities)), filteredPossibilities)
  }._1.toSeq.filter(_.promotionCodes.length > 1)

def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
  allCombinablePromotions(allPromotions)
    .filter(_.promotionCodes.contains(promotionCode))
