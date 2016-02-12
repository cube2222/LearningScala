/**
  * Created by Jakub Martin on 2/12/2016.
  */


trait RNG {
  def nextInt: (Int, RNG)
  def nonNegativeInt(): (Int, RNG)
  def double(): (Double, RNG)
  def intDouble(): ((Int, Double), RNG)
  def doubleInt(): ((Double, Int), RNG)
  def double3(rng: RNG): ((Double, Double, Double), RNG)
  def ints(count: Int):  (List[Int], RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

  def nonNegativeInt(): (Int, RNG) = {
    val (newVal, newRNG) = nextInt
    val newNonNegativeVal = math.abs(newVal % Int.MaxValue)
    return (newNonNegativeVal, newRNG)
  }

  def double(): (Double, RNG) = {
    val (newVal, newRNG) = nonNegativeInt
    (math.abs(newVal.toDouble / Int.MaxValue.toDouble), newRNG)
  }

  def intDouble(): ((Int, Double), RNG) = {
    ((nextInt._1, double._1), nextInt._2)
  }

  def doubleInt(): ((Double, Int), RNG) = {
    ((double._1, nextInt._1), nextInt._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    ((double._1, double._1, double._1), nextInt._2)
  }

  def ints(count: Int):  (List[Int], RNG) = {
    if(count > 0) (nextInt._1 :: nextInt._2.ints(count - 1)._1, nextInt._2.ints(count - 1)._2)
    else (List(nextInt._1), nextInt._2)
  }
}
