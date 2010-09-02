package uncertaintyMath

import scala.math._

object Measurement {

	implicit def intToMeasurement(x:Int):Measurement =
	{ new Measurement( x, Double.Epsilon) }
	implicit def doubleToMeasurement(x:Double):Measurement =
	{ new Measurement( x, Double.Epsilon ) }

	private def square(number: Double): Double =
	{ number*number }

	private def quadratureAddition(values: Double *): Double =
	{
			var sum: Double = 0.0
			values.foreach{value => sum += square(value)}
			math.sqrt(sum)
	}

	private def ln(value:Double): Double = {log(value)/log(E)}

	def minUncertain(that: Measurement): Measurement =
	{
		//forces Measurement to have >= minimum uncertainty
		val minimumUncertainty = Double.Epsilon
		//Previously 1E-4 based on experimental conditions for uncorrected library 
		if(math.abs(that.uncertainty) < minimumUncertainty)
			new Measurement(that.value,minimumUncertainty)
		else
			that
	}
	
	def ln(that:Measurement): Measurement = 
	{
			minUncertain(
					new Measurement( 
							ln(that.value),
							that.uncertainty/that.value
					)
			)
	}

	def sqrt(that:Measurement): Measurement =
	{ that^(.5)}

	def sin(that:Measurement): Measurement =
	{
			minUncertain(
					new Measurement( 
							math.sin(that.value),
							math.cos(that.value)*that.uncertainty
					)
			)
	}

	def cos(that:Measurement): Measurement =
	{
			minUncertain(
					new Measurement( 
							math.cos(that.value),
							-1*math.sin(that.value)*that.uncertainty
					)
			)
	}

	def tan(that:Measurement): Measurement =
	{
		val newValue = 	math.tan(that.value)
		minUncertain(
					new Measurement( 
							newValue,
							that.uncertainty*(1+square(newValue))
					)
			)
	}

	def asin(that:Measurement): Measurement =
	{
			minUncertain(
					new Measurement( math.asin(that.value),
							that.uncertainty/math.sqrt(1 - square(that.value))
					)
			)
	}

	def acos(that:Measurement): Measurement =
	{
			minUncertain(
					new Measurement( math.acos(that.value),
							-1*that.uncertainty/math.sqrt(1 - square(that.value))
					)
			)
	}

	def atan(that:Measurement): Measurement =
	{
			minUncertain(
					new Measurement( math.atan(that.value),
							that.uncertainty/quadratureAddition(1.0,that.value)
					)
			)
	}

	def atan2(numerator:Measurement, denominator:Measurement): Measurement =
	{
			minUncertain(
					new Measurement( math.atan2(numerator.value,denominator.value),
							quadratureAddition(numerator.value * denominator.uncertainty,
									denominator.value * numerator.uncertainty
							)
					)
			)
	}
}

class Measurement (val value: Double, val uncertainty:Double) extends Ordered[Measurement]
{
	import Measurement._

	def this(value:Double) = this(value,Double.Epsilon)
	def this(value:Int) = this(value,Double.Epsilon)

	//require(value != 0)//corrected uncertainty so that divide by zero will not occur

	def canEqual(other: Any): Boolean = { other.isInstanceOf[Measurement] }
	override def equals(other:Any):Boolean = 
	{
		other match {
		case other: Measurement => 
		{
			//the "2 *" is for 95% certainty
			(other canEqual this) && 
			(
					(this.value == other.value && this.uncertainty == other.uncertainty)
					||( abs(this.value - other.value) <= 
						2 * quadratureAddition(this.uncertainty,other.uncertainty) )
			)
		}
		case _ => false
		}
	}

	//only depends on value, not uncertainty
	override def hashCode: Int = { this.value.hashCode }

	override def toString = { this.value + " +- " + this.uncertainty }

	def compare(that: Measurement): Int = {
        //need to find out how to take uncertainty into account 
        if(this == that)
            return 0
        else if(this.value < that.value)
            return -1
        else
            return 1
    }
	
	def + (that:Int): Measurement =
	{
			minUncertain(
					new Measurement(
							this.value + that,
							this.uncertainty
					)
			)
	}
	def + (that:Measurement): Measurement = 
	{
			minUncertain(
					new Measurement(
							this.value + that.value,
							quadratureAddition(this.uncertainty,that.uncertainty)
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
	def - (that:Measurement): Measurement =
	{
			minUncertain(
					new Measurement(
							this.value - that.value,
							quadratureAddition(this.uncertainty,that.uncertainty)
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
	def * (that:Measurement): Measurement = 
	{
			val newValue: Double = this.value * that.value
			minUncertain(
					new Measurement( newValue,
							quadratureAddition(that.value * this.uncertainty,
									this.value * that.uncertainty
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
									newValue*power.uncertainty
							)
					)
			)
	}
}
