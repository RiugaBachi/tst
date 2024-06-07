import munit._
import org.scalacheck._
import org.scalacheck.Prop._
import best_price_finder._

class BestPriceSuite extends FunSuite with ScalaCheckSuite {
  def areEqualSets[T](xs: Seq[T], ys: Seq[T]) =
    (xs.toSet &~ ys.toSet).isEmpty

  def genRate(rateCodes: Seq[String]): Gen[Rate] = 
    for {
      rateCode <- Gen.oneOf(rateCodes)
      rateGroup <- Gen.identifier
    } yield Rate(rateCode, rateGroup)

  def genCabinPrice(rateCodes : Seq[String]): Gen[CabinPrice] = 
    for {
      rateCode <- Gen.oneOf(rateCodes)
      cabinCode <- Gen.identifier
      price <- Gen.oneOf(
        Gen.posNum : Gen[BigDecimal], 
        Gen.negNum : Gen[BigDecimal]
      )
    } yield CabinPrice(cabinCode, rateCode, price)

  def genInputs(rateCodes: Seq[String]): Gen[(List[Rate], List[CabinPrice])] = 
    Gen.zip(
      Gen.listOf(genRate(rateCodes)), 
      Gen.listOf(genCabinPrice(rateCodes))
    )

  def genRateCodes: Gen[List[String]] = 
    Gen.listOfN(5, Gen.identifier).suchThat(_.length > 0)

  test("example case") {
    val rates =
      Seq( Rate("M1", "Military")
         , Rate("M2", "Military")
         , Rate("S1", "Senior")
         , Rate("S2", "Senior")
         )

    val cabinPrices =
      Seq( CabinPrice("CA", "M1", 200.00)
         , CabinPrice("CA", "M2", 250.00)
         , CabinPrice("CA", "S1", 225.00)
         , CabinPrice("CA", "S2", 260.00)
         , CabinPrice("CB", "M1", 230.00)
         , CabinPrice("CB", "M2", 260.00)
         , CabinPrice("CB", "S1", 245.00)
         , CabinPrice("CB", "S2", 270.00)
         )

    val result = getBestGroupPrices(rates, cabinPrices)

    val expected =
      Seq( BestGroupPrice("CA", "M1", 200.00, "Military")
         , BestGroupPrice("CA", "S1", 225.00, "Senior")
         , BestGroupPrice("CB", "M1", 230.00, "Military")
         , BestGroupPrice("CB", "S1", 245.00, "Senior")
         )

    assert(areEqualSets(result, expected))
  }

  property("ouput values do not contain negative prices") {
    forAllNoShrink(genRateCodes) { rateCodes =>
      forAll(genInputs(rateCodes)) { (rates, prices) =>
        val result = getBestGroupPrices(rates, prices)

        result
          .map(_.price >= 0)
          .foldLeft(true)(_ && _)
      }
    }
  }

  // In other words, the outputs aren't criss-crossing values from input
  // pairs with different rateCodes
  property("ouput values are sourced from input pairs with like rate codes") {
    forAllNoShrink(genRateCodes) { rateCodes =>
      forAll(genInputs(rateCodes)) { (rates, prices) =>
        val result = getBestGroupPrices(rates, prices)

        result.map(x => {
          // There can be multiple `CabinPrice`s with the same rate code;
          // we only care that the result matches at least one of them.
          val outputMatchesInput = 
            for {
              inputRate <- rates.find(_.rateCode == x.rateCode)
              inputPrices <- Some(prices.filter(_.rateCode == x.rateCode))
            } yield (
              inputPrices.exists(_.cabinCode == x.cabinCode) &&
              inputPrices.exists(_.price == x.price) &&
              x.rateGroup == inputRate.rateGroup
            )

          outputMatchesInput.getOrElse(false)
        }).foldLeft(true)(_ && _)
      }
    }
  }

  property("all input rate groups appear in outputs, and vice versa") {
    forAllNoShrink(genRateCodes) { rateCodes =>
      forAll(genInputs(rateCodes)) { (rates, prices) =>
        val inputRateGroups = rates.map(_.rateGroup)

        val result = getBestGroupPrices(rates, prices)

        areEqualSets(result.map(_.rateGroup), inputRateGroups.sorted)
      }
    }
  }

  property("output contains only one best rate code per rate group") {
    forAllNoShrink(genRateCodes) { rateCodes =>
      forAll(genInputs(rateCodes)) { (rates, prices) =>
        val result = getBestGroupPrices(rates, prices)

        val groupedResult = result.groupMap(_.rateGroup)(_.rateCode)

        groupedResult.values
          .map(_.distinct.length == 1)
          .foldLeft(true)(_ && _)
      }
    }
  }

  property("all cabin codes for each rate code appear in output") {
    forAllNoShrink(genRateCodes) { rateCodes =>
      forAll(genInputs(rateCodes)) { (rates, prices) =>
        val sanitizedPrices = prices.filter(_.price >= 0)

        val cabinCodesByRateCode = sanitizedPrices.groupMap(_.rateCode)(_.cabinCode)

        val result = getBestGroupPrices(rates, prices)

        val groupedResult = result.groupMap(_.rateCode)(_.cabinCode)

        groupedResult.map((k, v) => 
          cabinCodesByRateCode
            .get(k)
            .map(_ == v)
            .getOrElse(false)
        ).foldLeft(true)(_ && _)
      }
    }
  }

  property("output rate codes have lowest prices within rate group") {
    forAllNoShrink(genRateCodes) { rateCodes =>
      forAll(genInputs(rateCodes)) { (rates, prices) =>
        val sanitizedRates = rates.distinctBy(_.rateCode)
        val sanitizedPrices = prices.filter(_.price >= 0)

        val inputPairs = 
          for {
            rate <- sanitizedRates.view
            price <- sanitizedPrices.view
            if price.rateCode == rate.rateCode
          } yield (rate, price)

        // Minimum of the sum of prices for each rate code, per rate group
        val groupedInput =
          inputPairs.groupBy(_._1.rateGroup).mapValues(
            _.groupMapReduce(_._2.rateCode)(_._2.price)(_ + _).values.min
          ).toMap

        val result = getBestGroupPrices(rates, prices)

        // Sum of prices per rate group
        val groupedResult = 
          result.groupMapReduce(_.rateGroup)(_.price)(_ + _)

        groupedResult.map((k, v) => 
          groupedInput.get(k).map(_ == v).getOrElse(false)
        ).foldLeft(true)(_ && _)
      }
    }
  }
}
