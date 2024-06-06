package best_price_finder

case class Rate(rateCode: String, rateGroup: String)

case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)

case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)

// TODO:
//  - Tech Debt: We should investigate whether or not we want to sanitize 
//  `price` fields in the input data via utilizing a (custom?) smart-constructed
//  BigDecimal wrapper type that disallows negative values. This would be cleaner 
//  than writing tests for these edge cases.
def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
  // We need to sanitize the input rates for errata as there could be two
  // `Rate`s with the same `rateCode` but different `rateGroup`s, which
  // breaks a fundamental assumption on the one-to-one relationship between
  // rateCodes and rateGroups; this could in turn lead to unexpected outputs.
  val uniqueRates = rates.distinctBy(_.rateCode)

  // We use Views here for lazy evaluation semantics.
  //
  // If we were to strictly evaluate the cartesian product of two Seqs of non-trivial size,
  // the resulting memory blowup could very likely OOM whatever environment this is running on.
  val allPossibilities = 
    for {
      rate <- uniqueRates.view
      price <- prices.view
      if price.rateCode == rate.rateCode
    } yield BestGroupPrice(price.cabinCode, rate.rateCode, price.price, rate.rateGroup)

  val rateCodesByRateGroup : Map[String, Seq[String]] = 
    uniqueRates.groupMap(_.rateGroup)(_.rateCode)
  
  // We have the option of sourcing rateCodes from `rates` or `prices`
  // For this exercise, let's assume the set of all unique rate codes in either sequence are equal.
  val totalPricesByRateCode : Map[String, BigDecimal] = 
    prices.groupMapReduce(_.rateCode)(_.price)(_ + _)

  val cheapestRateCodes : Seq[String] =
      rateCodesByRateGroup.values
        .flatMap(_.minByOption(x => totalPricesByRateCode.get(x)))
        .toSeq

  allPossibilities.filter(p => cheapestRateCodes.contains(p.rateCode)).toSeq
}
