package finalibre.valuation

object Extensions:
  extension(seq : Seq[Double])
    def sumPossiblyEmpty : Double = if seq.isEmpty then 0.0 else seq.sum


