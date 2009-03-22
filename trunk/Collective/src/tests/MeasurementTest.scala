package tests;

//import org.junit.Test
//import org.junit.Assert._
//import org.scalatest.Spec

import org.scalatest.junit.JUnit3Suite
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._


import UncertaintyMath._

//class MeasurementTest extends Spec with Checkers
class MeasurementTestSuite extends JUnit3Suite with Checkers
{
	
  //@Test
	def testSimpleEquality() {
		check((a: Double,b: Double,c: Double,d: Double) =>
		{(a == b)  ==> (new Measurement(a,c).value == new Measurement(b,d).value)}
		)
	}

	def testUncertainEquality() {
		check((a: Double,b: Double,c: Double,d: Double) =>
		{(a <= b+d || a >= b-d)  ==> (new Measurement(a,c) == new Measurement(b,d))}
		)
	}
 
}
