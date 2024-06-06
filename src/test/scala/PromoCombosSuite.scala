import munit._
import org.scalacheck._
import org.scalacheck.Prop._
import promotion_combos._

class PromoCombosSuite extends FunSuite with ScalaCheckSuite {
  def areEqualSets[T](xs: Seq[T], ys: Seq[T]) =
    (xs.toSet &~ ys.toSet).isEmpty

  def genPromotions: Gen[List[Promotion]] = 
    Gen.listOfN(12,
      for {
        codes <- Gen.listOf(Gen.identifier).suchThat(_.length > 0)
        notCombinableWith <- Gen.someOf(codes.tail)
      } yield Promotion(codes.head, notCombinableWith.toSeq)
    )

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

  test("input errata case: allCombinablePromotions, compatibility is checked fowards") {
    val promotions =
      Seq( Promotion("P1", Seq("P3"))
         , Promotion("P2", Seq())
         , Promotion("P3", Seq())
         )

    val result = allCombinablePromotions(promotions)

    val expected = 
      Seq( PromotionCombo(Seq("P1", "P2"))
         , PromotionCombo(Seq("P2", "P3"))
         )

    assert(areEqualSets(result, expected))
  }

  test("input errata case: allCombinablePromotions, compatibility is checked backwards") {
    val promotions =
      Seq( Promotion("P1", Seq())
         , Promotion("P2", Seq())
         , Promotion("P3", Seq("P1"))
         )

    val result = allCombinablePromotions(promotions)

    val expected = 
      Seq( PromotionCombo(Seq("P1", "P2"))
         , PromotionCombo(Seq("P2", "P3"))
         )

    assert(areEqualSets(result, expected))
  }

  property("allCombinablePromotions, all outputs are maximally combinable") {
    forAll(genPromotions) { promotion =>
      // A promotion combo being a subset of another means it is not maximally combinable
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

  property("combinablePromotions, all outputs contain required promotion") {
    forAll(genPromotions) { promotions =>
      //promotions.flatMap(p => 
      //  combinablePromotions(p.code, promotions)
      //    .map(_.promotionCodes.contains(p.code))
      //).foldLeft(true)(_ && _)
    }
  }

  property("combinablePromotions, outputs are a subset of allCombinablePromotions") {
    forAll(genPromotions) { promotions =>
      val allCombinable = allCombinablePromotions(promotions)
      println("here")
      promotions.map(p => 
        val combinableFor = combinablePromotions(p.code, promotions)
        combinableFor.toSet.subsetOf(allCombinable.toSet)
      ).foldLeft(true)(_ && _)
    }
  }

}
