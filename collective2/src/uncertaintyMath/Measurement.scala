package uncertaintyMath

import scala.math._

object Measurement {

	implicit def intToMeasurement(x:Int):Measurement =
	{ new Measurement( x, math.ulp(x.toDouble)) }
	implicit def doubleToMeasurement(x:Double):Measurement =
	{ new Measurement( x, math.ulp(x) ) }

	def apply(value: Double) = new Measurement(value,math.ulp(value ))
	def apply(value: Double, uncertainty: Double) = new Measurement(value,uncertainty)
	
	private def square(number: Double): Double =
	{ number*number }

	private def quadratureAddition(values: Double *): Double =
	{
			var sum: Double = 0.0
			values.foreach{value => sum += square(value)}
			math.sqrt(sum)
	}

	private def ln(value: Double): Double = math.log(value)
	//check whether log(E) is calculated or defined (performance optimization)

	def checkUncertain(that: Measurement, ulpFactor: Double): Measurement = {
		val minimumUncertainty = ulpFactor * math.ulp(that.value)
		//Previously 1E-4 based on experimental conditions for uncorrected library 
		if(that.uncertainty < minimumUncertainty)
			new Measurement(that.value,minimumUncertainty)
		else
			that
	}
	
	def minUncertain(that: Measurement): Measurement = checkUncertain(that,0.5)
	
	def minTranscendentalUncertain(that: Measurement): Measurement = {
		checkUncertain(that,1.0)
	}
	
	def ln(that:Measurement): Measurement = 
	{
			minTranscendentalUncertain(
					new Measurement( 
							ln(that.value),
							that.uncertainty/that.value
					)
			)
	}

	def log(that:Measurement): Measurement = ln(that)/ln(10)
	
	def exp(that:Measurement): Measurement = E^that
	
	def square(that: Measurement): Measurement = {
		minTranscendentalUncertain(that*that)	
	}
	def sqrt(that: Measurement): Measurement ={ 
		minTranscendentalUncertain(that^(.5))
	}

	def sin(that:Measurement): Measurement =
	{
			minTranscendentalUncertain(
					new Measurement( 
							math.sin(that.value),
							math.cos(that.value)*that.uncertainty
					)
			)
	}

	def cos(that:Measurement): Measurement =
	{
			minTranscendentalUncertain(
					new Measurement( 
							math.cos(that.value),
							-1*math.sin(that.value)*that.uncertainty
					)
			)
	}

	def tan(that:Measurement): Measurement =
	{
		val newValue = 	math.tan(that.value)
		minTranscendentalUncertain(
					new Measurement( 
							newValue,
							that.uncertainty*(1+square(newValue))
					)
			)
	}

	def asin(that:Measurement): Measurement =
	{
			minTranscendentalUncertain(
					new Measurement( math.asin(that.value),
							that.uncertainty/math.sqrt(1 - square(that.value))
					)
			)
	}

	def acos(that:Measurement): Measurement =
	{

		minTranscendentalUncertain(
			new Measurement( math.acos(that.value),
				that.uncertainty/math.sqrt(1 - square(that.value))
			)	
		)
			
	}

	def atan(that:Measurement): Measurement =
	{
			minTranscendentalUncertain(
					new Measurement( math.atan(that.value),
							that.uncertainty/quadratureAddition(1.0,that.value)
					)
			)
	}

	def atan2(numerator:Measurement, denominator:Measurement): Measurement =
	{
			checkUncertain(
					new Measurement( math.atan2(numerator.value,denominator.value),
							quadratureAddition(numerator.value * denominator.uncertainty,
									denominator.value * numerator.uncertainty
							)
					),2f
			)
	}
}

class Measurement(val value: Double, Uncertainty:Double) extends Ordered[Measurement] //with Numeric[Measurement]
{
	import Measurement._
	
	val uncertainty = if(this.Uncertainty > math.ulp(this.value ))
		this.Uncertainty
	else
		math.ulp(this.value )
	
	def this(value:Double) = this(value,math.ulp(value ))
	def this(value:Int) = this(value,math.ulp(value.toDouble ))
	
	//require(value != 0)//corrected uncertainty so that divide by zero will not occur

	def canEqual(other: Any): Boolean = { other.isInstanceOf[Measurement] ||
		 other.isInstanceOf[Double] || other.isInstanceOf[Int] 
	}
	override def equals(other:Any):Boolean = 
	// Measurement == (Int || Double) but (Int || Double) != Measurement
	{
		if(this.canEqual(other))
		{
				other match {
				case other: Measurement => {
					//the "2 *" is for 95% certainty
					(
							(this.value == other.value && this.uncertainty == other.uncertainty)
							||( abs(this.value - other.value) <= 
								2 * quadratureAddition(this.uncertainty,other.uncertainty) )
					)
				}
				case other: Double => this.equals(Measurement(other) )
				case other: Int => this.equals(Measurement(other) )
				case _ => false
				}
		}else
			false
	}

	//only depends on value, not uncertainty
	override def hashCode: Int = { this.value.hashCode }

	override def toString = { this.value + " +- " + this.uncertainty }

	def compare(x:Measurement, y:Measurement): Int = {
        //need to find out how to take uncertainty into account 
        if(x == y)
            return 0
        else if(x.value < y.value)
            return -1
        else
            return 1
    }
	def compare(that: Measurement): Int = {
        //need to find out how to take uncertainty into account 
        if(this == that)
            return 0
        else if(this.value < that.value)
            return -1
        else
            return 1
    }
	
	def toDouble (x: Measurement) : Double = x.value 
	def toFloat (x: Measurement) : Float = x.value.toFloat
	def toInt (x: Measurement) : Int = x.value.toInt
	def toLong (x: Measurement) : Long =x.value.toLong
	def fromInt (x: Int) : Measurement = Measurement(x)
	def negate(x: Measurement): Measurement = Measurement(-x.value,x.uncertainty )
	
	def + (that:Int): Measurement =
	{
			minUncertain(
					new Measurement(
							this.value + that,
							this.uncertainty
					)
			)
	}
	def + (that: Measurement) = plus(this,that)
	def plus(x:Measurement, y:Measurement): Measurement = 
	{
			minUncertain(
					new Measurement(
							x.value + y.value,
							quadratureAddition(x.uncertainty,y.uncertainty)
					)
			)
	}
	def - (that:Int): Measurement =
	{
			minUncertain(
					new Measurement(
							this.value - that,
							this.uncertainty
					)
			)
	}
	def - (that: Measurement) = minus(this,that)
	def minus(x:Measurement, y:Measurement): Measurement =
	{
			minUncertain(
					new Measurement(
							x.value - y.value,
							quadratureAddition(x.uncertainty,y.uncertainty)
					)
			)
	}

	def * (that:Int): Measurement = 
	{
			val newValue: Double = this.value * that
			minUncertain(
				new Measurement( newValue,that.value * this.uncertainty)
			)
	}
	def * (that: Measurement) = times(this,that)
	def times (x:Measurement, y:Measurement): Measurement = 
	{
			val newValue: Double = x.value * y.value
			minUncertain(
					new Measurement( newValue,
							quadratureAddition(y.value * x.uncertainty,
									x.value * y.uncertainty
							)
					)
			)
	}
	def / (that:Int): Measurement = 
	{
			val newValue: Double = this.value / that
			minUncertain(
					new Measurement( newValue,that.value * this.uncertainty)
			)
	}
	def / (that:Measurement): Measurement = 
	{
			val newValue: Double = this.value / that.value
			minUncertain(
					new Measurement( newValue,
							quadratureAddition(that.value * this.uncertainty,
									this.value * that.uncertainty
							)
					)
			)
	}

	def ^(power:Double): Measurement = 
	{
			val newValue: Double = pow(this.value,power)
			minUncertain(
					new Measurement( newValue,
							pow(this.value,power.value-1)*power.value*this.uncertainty
					)
			)
	}
	def ^(power:Measurement): Measurement = 
	{
			//d(x^b)=(x^b)[(b/x)dx + ln(x)db]
			val newValue: Double = pow(this.value,power.value)
			minUncertain(
					new Measurement( newValue,
							quadratureAddition(
									pow(this.value,power.value-1)*power.value*this.uncertainty,
									newValue*ln(this.value)*power.uncertainty
							)
					)
			)
	}
}
