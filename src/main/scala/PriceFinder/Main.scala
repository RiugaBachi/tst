import best_price_finder._

@main def main(): Unit = {
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
