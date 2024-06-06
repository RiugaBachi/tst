import best_price_finder._
import promotion_combos._

@main def printCombinations(): Unit = {
  val promotions =
    Seq( Promotion("P1", Seq("P3"))
       , Promotion("P2", Seq("P4", "P5"))
       , Promotion("P3", Seq("P1"))
       , Promotion("P4", Seq("P2"))
       , Promotion("P5", Seq("P2"))
       )

  println("1. All Promotion Combinations:")
  println(allCombinablePromotions(promotions) mkString "\n")

  println("\n2. Promotion Combinations for promotionCode=P1")
  println(combinablePromotions("P1", promotions) mkString "\n")
  
  println("\n3. Promotion Combinations for promotionCode=P3")
  println(combinablePromotions("P3", promotions) mkString "\n")
}

@main def printBestPrices(): Unit = {
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

  println(result mkString "\n")
}
