
import scala.Math._

object Measurement {

	implicit def intToMeasurement(x:Int):Measurement =
		{ new Measurement( x, 0) }
	implicit def doubleToMeasurement(x:Double):Measurement =
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
		var newUncertainty = {
            if(that.value != 0)
                that.uncertainty/that.value
            else
                that.uncertainty
        }
		if(Math.abs(newUncertainty) < 1E-04)
				newUncertainty = 1E-04
        new Measurement(ln(that.value),newUncertainty)
	}
 
	def sqrt(that:Measurement): Measurement =
		{ that^(.5)}
 
	def sin(that:Measurement): Measurement =
	{
			var propogatedUncertainty: Double = Math.cos(that.value)*that.uncertainty
			if(Math.abs(propogatedUncertainty) < 1E-04)
				propogatedUncertainty = 1E-04
			new Measurement( Math.sin(that.value),propogatedUncertainty)
	}

	def cos(that:Measurement): Measurement =
	{
			var propogatedUncertainty: Double = -1*Math.sin(that.value)*that.uncertainty
			if(Math.abs(propogatedUncertainty) < 1E-04)
				propogatedUncertainty = 1E-04
			new Measurement( Math.cos(that.value),propogatedUncertainty)
	}
 
	def tan(that:Measurement): Measurement =
	{
			var propogatedUncertainty: Double = that.uncertainty/square(Math.cos(that.value))
			if(Math.abs(propogatedUncertainty) < 1E-04)
				propogatedUncertainty = 1E-04
			new Measurement( Math.tan(that.value),propogatedUncertainty)
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
 
	def atan2(numerator:Measurement, denominator:Measurement): Measurement =
	{
			new Measurement( Math.atan2(numerator.value,denominator.value),
                if(denominator.value != 0){
                  val fraction = numerator/denominator
                	(numerator/denominator).uncertainty/quadratureAddition(1.0,fraction.value)
                }
                else{
                	if(numerator.uncertainty > denominator.uncertainty)
                		numerator.uncertainty
                	else
                		denominator.uncertainty
                }
				
			)
	}
}

class Measurement (val value: Double, val uncertainty:Double) extends Ordered[Measurement]
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
		  case int: Int => {
			  this.value == int && this.uncertainty == 0
		  }
		  case dbl: Double => {
			  this.value == dbl && this.uncertainty == 0
		  }
          case _ => false
		}
	}
 
	//only depends on value, not uncertainty
	override def hashCode: Int = { this.value.hashCode }
 
	override def toString = { this.value + " +- " + this.uncertainty }

    def compare(that: Measurement): Int = {
        //need to find out how to take into account uncertainty
        if(this == that)
            return 0
        else if(this.value < that.value)
            return -1
        else
            return 1
    }

	def + (that:Int): Measurement =
	{
		var propogatedUncertainty = this.uncertainty
		if(Math.abs(propogatedUncertainty) < 1E-04)
				propogatedUncertainty = 1E-04
		new Measurement(
				this.value + that,propogatedUncertainty)
	}
	def + (that:Measurement): Measurement = 
	{
		var propogatedUncertainty = quadratureAddition(this.uncertainty,that.uncertainty)
		if(Math.abs(propogatedUncertainty) < 1E-04)
				propogatedUncertainty = 1E-04
		new Measurement(
				this.value + that.value,propogatedUncertainty)
	}
	def - (that:Int): Measurement =
	{
		var propogatedUncertainty = this.uncertainty
		if(Math.abs(propogatedUncertainty) < 1E-04)
				propogatedUncertainty = 1E-04
		new Measurement(
				this.value - that,propogatedUncertainty)
	}
	def - (that:Measurement): Measurement =
	{
		var propogatedUncertainty = quadratureAddition(this.uncertainty,that.uncertainty)
		if(Math.abs(propogatedUncertainty) < 1E-04)
				propogatedUncertainty = 1E-04
		new Measurement(
				this.value - that.value,propogatedUncertainty)
	}

	def * (that:Int): Measurement = 
	{
		val newValue: Double = this.value * that
        var newUncertainty: Double = {
            if(this.value != 0)
                newValue * this.uncertainty/this.value
            else
                this.uncertainty
        }
        if(Math.abs(newUncertainty) < 1E-04)
				newUncertainty = 1E-04
		new Measurement( newValue, newUncertainty)
	}
	def * (that:Measurement): Measurement = 
	{
		val newValue: Double = this.value * that.value
        var newUncertainty: Double = {
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
        if(Math.abs(newUncertainty) < 1E-04)
				newUncertainty = 1E-04
		new Measurement( newValue,newUncertainty)
	}
	def / (that:Int): Measurement = 
	{
		val newValue: Double = this.value / that
		var newUncertainty: Double = {
            if(this.value != 0)
                newValue * this.uncertainty/this.value
            else
                this.uncertainty
        }
		if(Math.abs(newUncertainty) < 1E-04)
				newUncertainty = 1E-04
		new Measurement( newValue, newUncertainty)
	}
	def / (that:Measurement): Measurement = 
	{
		val newValue: Double = this.value / that.value
		var newUncertainty: Double = {
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
		if(Math.abs(newUncertainty) < 1E-04)
			newUncertainty = 1E-04
		new Measurement( newValue,newUncertainty)
	}

	def ^(power:Double): Measurement = 
	{
		val newValue: Double = pow(this.value,power)
        var newUncertainty: Double = {
            if(this.value != 0)
                newValue*power*this.uncertainty/this.value
            else
                this.uncertainty
        }
        if(Math.abs(newUncertainty) < 1E-04)
			newUncertainty = 1E-04
		new Measurement( newValue,newUncertainty)
	}
	def ^(power:Measurement): Measurement = 
	{
	    //correct if d(x^b)=(x^b)[(b/x)dx + ln(x)db]
	    val newValue: Double = pow(this.value,power.value)
        var newUncertainty: Double = {
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
        if(Math.abs(newUncertainty) < 1E-04)
			newUncertainty = 1E-04
		new Measurement( newValue,newUncertainty)
	}
}


