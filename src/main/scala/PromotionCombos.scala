package promotion_combos

import scala.util.chaining.scalaUtilChainingOps

case class Promotion(code: String, notCombinableWith: Seq[String])

// We leverage the fact that we are defining a type that wraps Seq[String]
// and can define our own instances (overrides) of certain traits.
//
// In this particular case, within the bounds of the assignment,
// it makes sense for a PromotionCombo to have special equality
// properties. Namely, that it is set-like. The ordering of the
// internal promotion codes does not matter for two PromotionCombos 
// to be equal.
case class PromotionCombo(promotionCodes: Seq[String]) {
  override def equals(that: Any) =
    that match {
      case combo : PromotionCombo => (this.promotionCodes.toSet &~ combo.promotionCodes.toSet).isEmpty
      case _ => false
    }
  
  // We can leverage bitwise operator laws to splice hashcodes in a manner that doesn't
  // respect ordering.
  //
  // Probably a more idiomatic way of doing this though; just followed
  // my intuition here.
  //
  // Necessary since Set may default to HashSet and hashCodes will be compared before `equals`
  override def hashCode() =
    this.promotionCodes.foldLeft(0)(_.hashCode ^ _.hashCode)
}

def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
  // Recursively find combinable chains of promotions, starting from an initial promotion
  // and branching into compatible (combinable) remaining promotions.
  //
  // An input data edge case we have to consider here is that just because Promotion A
  // has Promotion B in its notCombinableWith, does not guarantee that Promotion B
  // has Promotion A in its notCombinableWith. It _should_, but the types as-is
  // do not guarantee this invariant.
  //
  // As a result, we need to play it safe and check compatibility "backwards" 
  // as well as "forwards" as we traverse the input data and build each chain.
  def findPromotionChains(
    promotionCodesInChain: Set[String],
    promotionToAdd: Promotion,
    remainingPromotions: Set[Promotion]
  ): Option[Set[PromotionCombo]] = {
    val isBackwardsCompatible = 
      (promotionCodesInChain & promotionToAdd.notCombinableWith.toSet).isEmpty

    // Base case #1: End the chain as a dead end (None) if the current promotion isn't backwards compatible
    // with what is already in the chain.
    if !isBackwardsCompatible then
      return None

    val forwardsCompatiblePromotions = 
      remainingPromotions.filter(p => !promotionToAdd.notCombinableWith.contains(p.code))

    // Base case #2: End the chain and return its promotion codes (inclusive of the current promotion) if 
    // there are no more remaining promotions that are be compatible with the chain to traverse.
    if forwardsCompatiblePromotions.isEmpty then
      return Some(Set(PromotionCombo((promotionCodesInChain + promotionToAdd.code).toSeq)))

    // Recursive case: Explore the remaining combinable promotions as individual chains and flatten
    // the resulting chains (sets).
    //
    // If all paths dead-end (i.e. all are None), return the current chain up to this point (inclusive)
    //
    // N.B. I would have liked to use something like Haskell's `sequence` here, but apparently this only exists in ScalaZ.
    // I didn't want to add a dependency just for it so this is slightly less than ideal way of expressing this.
    val subchains =
      forwardsCompatiblePromotions
        .flatMap(p => 
          findPromotionChains(
            promotionCodesInChain + promotionToAdd.code, 
            p, 
            forwardsCompatiblePromotions.excl(p)
          )
        )
        .flatten
    
    if subchains.isEmpty then 
      Some(Set(PromotionCombo((promotionCodesInChain + promotionToAdd.code).toSeq)))
    else Some(subchains)
  }

  val sanitizedPromotions = {
    // There may be duplicate promotions in the input; we want to combine their notCombinableWiths
    // to err on the side of caution
    def mergeDuplicates(xs: Seq[Promotion]) = 
      xs.groupMapReduce(_.code)(x => x)((x, y) => 
        Promotion(x.code, x.notCombinableWith ++ y.notCombinableWith)
      ).values

    // There may be self-conflicting Promotions in the input (i.e. a promotion with its own code in the notCombinableWith list)
    // We want to disregard these entirely.
    def discardSelfNoncombinables(xs: Iterable[Promotion]) = 
      xs.filter(p => !p.notCombinableWith.contains(p.code))
    
    allPromotions
      .pipe(mergeDuplicates)
      .pipe(discardSelfNoncombinables)
      .toSet
  }

  sanitizedPromotions.flatMap { p =>
    findPromotionChains(Set.empty, p, sanitizedPromotions.excl(p))
      .getOrElse(Set.empty)
  }.toSeq.filter(_.promotionCodes.length > 1)

// N.B This is not optimal as we need to first compute all combinable promotions before filtering for
// the ones we care about.
//
// This implementation is, however, simple; this in turn also keeps our test suite simple.
//
// If we use this function frequently in production and wanted to reduce the time complexity,
// we should look into refactoring the logic from allCombinablePromotions into a private helper function
// internal to this package that, say, returns a map of "root" promotion codes to a lazy context we can evaluate
// to obtain the combinable promotions for that particular root promotion. `allCombinablePromotions` would then
// evaluate all these contexts and merge the resultant sets (kind of like what it is doing now at its tail),
// while `combinablePromotions` can look up the context it cares about and evaluate just that.
//
// Another idea would be to move `findPromotionChains` and the promotion sanitization logic out to be used in
// `allCombinablePromotions` and `combinablePromotions` independently.
//
// I didn't roll with the aforementioned implementations as it adds complexity without clear hard requirements that 
// justify the need for it.
def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
  allCombinablePromotions(allPromotions)
    .filter(_.promotionCodes.contains(promotionCode))
