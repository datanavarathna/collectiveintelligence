package tests

import org.scalatest.junit.JUnitSuite
import org.junit._
import Assert._

import focusedDstar._
import scala.collection.mutable

class DstarTest extends JUnitSuite{
	var factory = new CoordinateCreator((0,0),(2,2))
	var start: Coordinate = null
	var goal: Coordinate = null
	
	@Test def noInfiniteRecursion {
		factory = new CoordinateCreator((0,0),(2,2))
		//create timer actor to fail test if takes too long
		var startTime = System.currentTimeMillis
		start = factory.createCoordinate(0, 0)
		goal = factory.createCoordinate(2, 2)
		
	}
}