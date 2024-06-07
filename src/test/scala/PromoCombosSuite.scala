import munit._
import org.scalacheck._
import org.scalacheck.Test.{ Parameters }
import org.scalacheck.Prop._
import scala.util.Random
import promotion_combos._

class PromoCombosSuite extends FunSuite with ScalaCheckSuite {
  def areEqualSets[T](xs: Seq[T], ys: Seq[T]) =
    (xs.toSet &~ ys.toSet).isEmpty

  def genPromotions: Gen[List[Promotion]] = 
    Gen.listOfN(10,
      for {
        codes <- Gen.listOfN(10, Gen.identifier)
        notCombinableWith <- Gen.someOf(codes.tail)
      } yield Promotion(codes.head, notCombinableWith.toSeq)
    )

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(1)

  /***************************
   * allCombinablePomotions
   ***************************/

  test("example case: allCombinablePromotions") {
    val promotions =
      Seq( Promotion("P1", Seq("P3"))
         , Promotion("P2", Seq("P4", "P5"))
         , Promotion("P3", Seq("P1"))
         , Promotion("P4", Seq("P2"))
         , Promotion("P5", Seq("P2"))
         )

    val result = allCombinablePromotions(promotions)

    val expected =
      Seq( PromotionCombo(Seq("P1", "P2"))
         , PromotionCombo(Seq("P1", "P4", "P5"))
         , PromotionCombo(Seq("P2", "P3"))
         , PromotionCombo(Seq("P3", "P4", "P5"))
         )

    assert(areEqualSets(result, expected))
  }

  test("edge case: allCombinablePromotions, a promotion not combinable with itself is disregarded") {
    val promotions =
      Seq( Promotion("P1", Seq("P1"))
         , Promotion("P2", Seq())
         , Promotion("P3", Seq())
         )

    val result = allCombinablePromotions(promotions)

    val expected = 
      Seq( PromotionCombo(Seq("P2", "P3"))
         )

    assert(areEqualSets(result, expected))
  }

  test("edge case: allCombinablePromotions, in the event of a duplicate promotion, their non-combinable promotions are merged") {
    val promotions =
      Seq( Promotion("P1", Seq("P2"))
         , Promotion("P1", Seq())
         , Promotion("P2", Seq())
         , Promotion("P3", Seq())
         )

    val result = allCombinablePromotions(promotions)

    val expected = 
      Seq( PromotionCombo(Seq("P1", "P3"))
         , PromotionCombo(Seq("P2", "P3"))
         )

    assert(areEqualSets(result, expected))
  }

  property("allCombinablePromotions, all output promotion codes exist in inputs, and vice versa") {
    forAll(genPromotions) { promotions =>
      val result = 
        allCombinablePromotions(promotions)
          .map(_.promotionCodes.toSet)
          .foldLeft(Set.empty: Set[String])(_.union(_))

      (result &~ promotions.map(_.code).toSet).isEmpty
    }
  }

  property("allCombinablePromotions, promotion combos have more than 1 promotion code") {
    forAll(genPromotions) { promotions =>
      val result = 
        allCombinablePromotions(promotions)
          .map(_.promotionCodes.length > 1)

      result.foldLeft(true)(_ && _)
    }
  }

  property("allCombinablePromotions, all promo codes within promo combos are combinable with one another") {
    forAll(genPromotions) { promotions =>
      val nonCombinablesMap = promotions.groupMapReduce(_.code)(_.notCombinableWith)(_ ++ _)

      val result = allCombinablePromotions(promotions)

      result.map(combo =>
        val codes = combo.promotionCodes.toSet
        val nonCombinables = codes.flatMap(c => nonCombinablesMap.get(c).map(_.toSet).getOrElse(Set.empty))

        (codes & nonCombinables).isEmpty
      ).foldLeft(true)(_ && _)
    }
  }

  property("allCombinablePromotions, all promo combos are maximally combinable") {
    forAll(genPromotions) { promotions =>
      val result = allCombinablePromotions(promotions)

      val sanitizedPromotions = 
        promotions.groupMapReduce(_.code)(x => x)((x, y) => 
          Promotion(x.code, x.notCombinableWith ++ y.notCombinableWith)
        ).values

      result.map(combo =>
        val codes = combo.promotionCodes.toSet

        val promotionsIncluded = sanitizedPromotions.filter(p => codes.contains(p.code))
        val promosNotIncluded = sanitizedPromotions.filter(p => !codes.contains(p.code))

        val internalNonCombinables = promotionsIncluded.flatMap(_.notCombinableWith).toSet

        val allAreMaximal =
          promosNotIncluded.map(p => {
            // An unincluded promotion is considered externally incompatible if:
            //  1. It is self-contradicting (not combinable with itself)
            //  2. Its own notCombinableWith contains a conflict with the promotion codes in the combo
            val externallyIncompatible = p.notCombinableWith.contains(p.code) || !(p.notCombinableWith.toSet & codes).isEmpty
            // An unincluded promotion is considered internally incompatible if the set of 
            // noncombinable promotions within the combo conflicts with the unincluded promotion.
            val internallyIncompatible = internalNonCombinables.contains(p.code)
            // To prove that an unincluded promotion could not have been included,
            // we need to establish the presence of either an external or internal incompatibility.
            val couldNotBeIncluded = externallyIncompatible || internallyIncompatible

            couldNotBeIncluded
          }).foldLeft(true)(_ && _)

        allAreMaximal
      ).foldLeft(true)(_ && _)
    }
  }

  /************************
   * combinablePomotions
   ************************/

  test("example case: combinablePromotions, promotionCode=P1") {
    val promotions =
      Seq( Promotion("P1", Seq("P3"))
         , Promotion("P2", Seq("P4", "P5"))
         , Promotion("P3", Seq("P1"))
         , Promotion("P4", Seq("P2"))
         , Promotion("P5", Seq("P2"))
         )

    val result = combinablePromotions("P1", promotions)

    val expected =
      Seq( PromotionCombo(Seq("P1", "P2"))
         , PromotionCombo(Seq("P1", "P4", "P5"))
         )

    assert(areEqualSets(result, expected))
  }

  test("example case: combinablePromotions, promotionCode=P3") {
    val promotions =
      Seq( Promotion("P1", Seq("P3"))
         , Promotion("P2", Seq("P4", "P5"))
         , Promotion("P3", Seq("P1"))
         , Promotion("P4", Seq("P2"))
         , Promotion("P5", Seq("P2"))
         )

    val result = combinablePromotions("P3", promotions)

    val expected =
      Seq( PromotionCombo(Seq("P3", "P2"))
         , PromotionCombo(Seq("P3", "P4", "P5"))
         )

    assert(areEqualSets(result, expected))
  }

  test("edge case: combinablePromotions, nonexistent promotion code returns no results") {
    val promotions =
      Seq( Promotion("P1", Seq())
         , Promotion("P2", Seq())
         , Promotion("P3", Seq())
         )

    val result = combinablePromotions("X", promotions)

    assert(areEqualSets(result, Seq()))
  }

  property("combinablePromotions, all outputs contain the required promotion") {
    forAll(genPromotions.suchThat(_.length > 0)) { promotions =>
      forAll(Gen.oneOf(promotions)) { somePromo =>
        combinablePromotions(somePromo.code, promotions)
          .map(_.promotionCodes.contains(somePromo.code))
          .foldLeft(true)(_ && _)
      }
    }
  }

  property("combinablePromotions, outputs contain every promotion combo from allCombinablePromotions with the required promotion") {
    forAll(genPromotions.suchThat(_.length > 0)) { promotions =>
      val allCombinables = allCombinablePromotions(promotions)
      forAll(Gen.oneOf(promotions)) { somePromo =>
        val combinablesForSomePromo = combinablePromotions(somePromo.code, promotions)
        
        val numMatchingOutputsFromAllCombinables = allCombinables.filter(_.promotionCodes.contains(somePromo.code)).length
        val numOutputsFromCombinables = combinablesForSomePromo.length

        numOutputsFromCombinables == numMatchingOutputsFromAllCombinables
      }
    }
  }

  // If the following property holds, so too will several properties from allCombinablePromotions
  // hold for combinablePromotions by implication; namely:
  //  - all output promo combos are maximal
  //  - all output promo combos have more than one promotion code
  property("combinablePromotions, outputs are a subset of allCombinablePromotions") {
    forAll(genPromotions.suchThat(_.length > 0)) { promotions =>
      val allCombinables = allCombinablePromotions(promotions)
      forAll(Gen.oneOf(promotions)) { somePromo =>
        val combinablesForSomePromo = combinablePromotions(somePromo.code, promotions)

        combinablesForSomePromo.toSet.subsetOf(allCombinables.toSet)
      }
    }
  }
}
