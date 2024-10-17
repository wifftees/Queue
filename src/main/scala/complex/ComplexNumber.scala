package complex

import scala.language.implicitConversions

// DO NOT CHANGE ANYTHING BELOW
final case class ComplexNumber(real: Double, imaginary: Double) {
  def *(other: ComplexNumber) =
    ComplexNumber(
      (real * other.real) - (imaginary * other.imaginary),
      (real * other.imaginary) + (imaginary * other.real)
    )
  def +(other: ComplexNumber) =
    ComplexNumber(real + other.real, imaginary + other.imaginary)
  def ~=(o: ComplexNumber) =
    (real - o.real).abs < 1e-6 && (imaginary - o.imaginary).abs < 1e-6
}

object ComplexNumber {
  // DO NOT CHANGE ANYTHING ABOVE
}

object ComplexNumberExtended {
  final case class PolarForm(r: Double, theta: Double)

  implicit class ComplexNumberExtended(val number: ComplexNumber) extends Numeric[ComplexNumber] { self =>
    def -(other: ComplexNumber): ComplexNumber =
      ComplexNumber(number.real - other.real, number.imaginary - other.imaginary)

    def /(other: ComplexNumber): ComplexNumber = if (other.real == 0 && other.imaginary == 0)
      throw new ArithmeticException("Divisor should not be zero")
    else
      ComplexNumber(
        (number.real * other.real + number.imaginary * other.imaginary) / (Math.pow(number.real, 2) + Math
          .pow(other.real, 2)),
        (number.imaginary * other.real - number.real * other.imaginary) / (Math.pow(number.real, 2) + Math
          .pow(other.real, 2))
      )

    def toPolarForm: PolarForm = PolarForm(
      Math.sqrt(Math.pow(number.real, 2) + Math.pow(number.imaginary, 2)),
      Math.atan(number.imaginary / number.real)
    )

    def i: ComplexNumber = ComplexNumber(0, number.real)

    override def plus(x: ComplexNumber, y: ComplexNumber): ComplexNumber = x + y

    override def minus(x: ComplexNumber, y: ComplexNumber): ComplexNumber = ComplexNumberExtended(x) - y

    override def times(x: ComplexNumber, y: ComplexNumber): ComplexNumber = x * y

    override def negate(x: ComplexNumber): ComplexNumber = ComplexNumber(-x.real, -x.imaginary)

    override def fromInt(x: Int): ComplexNumber = ComplexNumber(x.toDouble, 0)

    // string in format: "1 + 2i"
    override def parseString(str: String): Option[ComplexNumber] = str.trim.split(" + ").toList match {
      case real :: imaginary :: Nil =>
        real.toIntOption.flatMap(realInt =>
          imaginary.toIntOption.map(realImaginary => ComplexNumber(realInt.toDouble, realImaginary.toDouble))
        )
      case _ => None
    }

    override def toInt(x: ComplexNumber): Int = x.real.toInt;

    override def toLong(x: ComplexNumber): Long = x.real.toLong;

    override def toFloat(x: ComplexNumber): Float = x.real.toFloat

    override def toDouble(x: ComplexNumber): Double = x.real

    override def compare(x: ComplexNumber, y: ComplexNumber): Int = (x.real - y.real).toInt
  }

  implicit def toComplexNumber(x: Int): ComplexNumberExtended = ComplexNumber(x.toDouble, 0)
}
