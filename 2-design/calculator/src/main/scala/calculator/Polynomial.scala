package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal {
      val aval = a()
      val bval = b()
      val cval = c()
      bval * bval - 4 * aval * cval
    }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      val aval = a()
      val bval = b()
      val deltaSqrt = Math.sqrt(computeDelta(a, b, c)())
      if (deltaSqrt >= 0) {
        Set((-1 * bval + deltaSqrt) / (2 * aval),
            (-1 * bval - deltaSqrt) / (2 * aval))
      }
      else Set.empty
    }
