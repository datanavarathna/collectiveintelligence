
import scala.Math._

object Measurement {

	implicit def intToMeasurement(x:Int):Measurement =
		{ new Measurement( x, 0) }
	implicit def intToMeasurement(x:Double):Measurement =
		{ new Measurement( x, 0) }
  
	private def square(number: Double): Double =
	{ number*number }

	private def quadratureAddition(values: Double *): Double =
	{
			var sum: Double = 0.0
			values.foreach{value => sum += square(value)}
	        Math.sqrt(sum)
	}

	private def ln(value:Double): Double = {log(value)/log(E)}

	def ln(that:Measurement): Measurement = 
	{
		val newUncertainty = {
            if(that.value != 0)
                that.uncertainty/that.value
            else
                that.uncertainty
        }
        new Measurement(ln(that.value),newUncertainty)
	}
 
	def sqrt(that:Measurement): Measurement =
		{ that^(.5)}
 
	def sin(that:Measurement): Measurement =
	{
			new Measurement( Math.sin(that.value),
					Math.cos(that.value)*that.uncertainty
			)
	}

	def cos(that:Measurement): Measurement =
	{
			new Measurement( Math.cos(that.value),
					-1*Math.sin(that.value)*that.uncertainty
			)
	}
 
	def tan(that:Measurement): Measurement =
	{
			new Measurement( Math.tan(that.value),
				that.uncertainty/square(Math.cos(that.value))
			)
	}
 
	def asin(that:Measurement): Measurement =
	{
			new Measurement( Math.asin(that.value),
				that.uncertainty/Math.sqrt(1 - square(that.value))
			)
	}
 
	def acos(that:Measurement): Measurement =
	{
			new Measurement( Math.acos(that.value),
				-1*that.uncertainty/Math.sqrt(1 - square(that.value))
			)
	}
 
	def atan(that:Measurement): Measurement =
	{
			new Measurement( Math.atan(that.value),
				that.uncertainty/quadratureAddition(1.0,that.value)
			)
	}
}

class Measurement (val value: Double, val uncertainty:Double) 
{
	import Measurement._
	
	def this(value:Double) = this(value,0)
	def this(value:Int) = this(value,0)
 
	//require(value != 0)
 
	def canEqual(other: Any): Boolean = { other.isInstanceOf[Measurement] }
	override def equals(other:Any):Boolean = 
	{
		other match {
		  case other: Measurement => 
		    {
		      //the "2 *" is for 95% certainty
		      (other canEqual this) &&
              ( abs(this.value - other.value) <= 
				2 * quadratureAddition(this.uncertainty,other.uncertainty) )
		    }
          case _ => false
		}
	}
 
	//only depends on value, not uncertainty
	override def hashCode: Int = { this.value.hashCode }
 
	override def toString = { this.value + " +- " + this.uncertainty }
 
	def + (that:Int): Measurement =
	{
		new Measurement(
				this.value + that,
				this.uncertainty
		)
	}
	def + (that:Measurement): Measurement = 
	{
		new Measurement(
				this.value + that.value,
				quadratureAddition(this.uncertainty,that.uncertainty)
		)
	}
	def - (that:Int): Measurement =
	{
		new Measurement(
				this.value - that,
				this.uncertainty
		)
	}
	def - (that:Measurement): Measurement =
	{
		new Measurement(
				this.value - that.value,
				quadratureAddition(this.uncertainty,that.uncertainty)
		)
	}

	def * (that:Int): Measurement = 
	{
		val newValue: Double = this.value * that
        val newUncertainty: Double = {
            if(this.value != 0)
                newValue * this.uncertainty/this.value
            else
                this.uncertainty
        }
		new Measurement( newValue, newUncertainty)
	}
	def * (that:Measurement): Measurement = 
	{
		val newValue: Double = this.value * that.value
        val newUncertainty: Double = {
            if(this.value != 0)
                newValue * quadratureAddition(
                    this.uncertainty/this.value,
					that.uncertainty/this.value
              )
            else{
                if(this.uncertainty > that.uncertainty)
                    this.uncertainty
                else
                    that.uncertainty
            }
        }
		new Measurement( newValue,newUncertainty)
	}
	def / (that:Int): Measurement = 
	{
		val newValue: Double = this.value / that
		val newUncertainty: Double = {
            if(this.value != 0)
                newValue * this.uncertainty/this.value
            else
                this.uncertainty
        }
		new Measurement( newValue, newUncertainty)
	}
	def / (that:Measurement): Measurement = 
	{
		val newValue: Double = this.value / that.value
		val newUncertainty: Double = {
            if(this.value != 0)
                newValue * quadratureAddition(
                    this.uncertainty/this.value,
					that.uncertainty/this.value
              )
            else{
                if(this.uncertainty > that.uncertainty)
                    this.uncertainty
                else
                    that.uncertainty
            }
        }
		new Measurement( newValue,newUncertainty)
	}

	def ^(power:Double): Measurement = 
	{
		val newValue: Double = pow(this.value,power)
        val newUncertainty: Double = {
            if(this.value != 0)
                newValue*power*this.uncertainty/this.value
            else
                this.uncertainty
        }
		new Measurement( newValue,newUncertainty)
	}
	def ^(power:Measurement): Measurement = 
	{
	    //correct if d(x^b)=(x^b)[(b/x)dx + ln(x)db]
	    val newValue: Double = pow(this.value,power.value)
        val newUncertainty: Double = {
            if(this.value != 0)
                newValue *  ( power.value*this.uncertainty/this.value +
						ln(this.value)*power.uncertainty )
            else{
                if(this.uncertainty > power.uncertainty)
                    this.uncertainty
                else
                    power.uncertainty
            }
        }
		new Measurement( newValue,newUncertainty)
	}
}


