package best_price_finder

case class Rate(rateCode: String, rateGroup: String)

case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)

case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)

def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
  // We use Views here for lazy evaluation semantics.
  //
  // If we were to strictly evaluate the cartesian product of two Seqs of non-trivial size,
  // the resulting memory blowup could very likely OOM whatever environment this is running on.
  val allPossibilities = 
    for {
      rate <- rates.view
      price <- prices.view
      if price.rateCode == rate.rateCode
    } yield BestGroupPrice(price.cabinCode, rate.rateCode, price.price, rate.rateGroup)

  val rateCodesByRateGroup : Map[String, Seq[String]] = 
    rates.groupBy(_.rateGroup).mapValues(_.map(_.rateCode)).toMap
  
  // We have the option of sourcing rateCodes from `rates` or `prices`
  // For this exercise, let's assume the set of all unique rate codes in either sequence are equal.
  val totalPricesByRateCode : Map[String, BigDecimal] = 
    prices.groupBy(_.rateCode).mapValues(_.map(_.price).sum).toMap

  val cheapestRateCodes : Seq[String] =
      rateCodesByRateGroup.values
        .map(_.minByOption(x => totalPricesByRateCode.get(x)))
        .flatten
        .toSeq

  allPossibilities.filter(p => cheapestRateCodes.contains(p.rateCode)).toSeq
}
