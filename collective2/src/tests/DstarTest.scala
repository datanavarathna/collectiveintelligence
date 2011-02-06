package tests

import org.scalatest.junit.JUnitSuite
import org.junit._
import Assert._

import focusedDstar._
import scala.collection.mutable

import scala.actors.Actor
import scala.actors.Actor._

class DstarTest extends JUnitSuite{
	var factory = new CoordinateCreator((0,0),(2,2))
	var start: Coordinate = null
	var goal: Coordinate = null
	
	@Test def noInfiniteRecursion {
		val msTimeout: Long = 2000
		//create timer actor to fail test if takes too long
		var timer = actor {
			var startTime = System.currentTimeMillis
			reactWithin(msTimeout){
				case 'finished => exit
				case _ => {
					print(System.currentTimeMillis - startTime)
					fail
					exit
				}
			}
		}
		start = factory.createCoordinate(0, 0)
		goal = factory.createCoordinate(2, 2)
		timer ! 'finished
	}
	
	@Test def neighbors{
		var adjacent = factory.getCoordinate(0, 1)::
			factory.getCoordinate(1, 0)::
			factory.getCoordinate(1, 1):: Nil
		var coordNeighbors = factory.getCoordinate(0, 0).neighbors
		assert(adjacent.length === coordNeighbors.length)
		assertTrue(adjacent+"!="+coordNeighbors,coordNeighbors.sameElements(adjacent))
		adjacent = factory.getCoordinate(1, 1)::
			factory.getCoordinate(1, 2)::
			factory.getCoordinate(2, 1):: Nil
		coordNeighbors = factory.getCoordinate(2, 2).neighbors
		assert(adjacent.length === coordNeighbors.length)
		assertTrue(adjacent+"!="+coordNeighbors,coordNeighbors.sameElements(adjacent))
		adjacent = factory.getCoordinate(0, 0)::
			factory.getCoordinate(0, 1)::
			factory.getCoordinate(0, 2)::
			factory.getCoordinate(1, 0)::
			factory.getCoordinate(1, 2)::
			factory.getCoordinate(2, 0)::
			factory.getCoordinate(2, 1)::
			factory.getCoordinate(2, 2)::Nil
		coordNeighbors = factory.getCoordinate(1, 1).neighbors
		assert(adjacent.length === coordNeighbors.length)
		assertTrue(adjacent+"!="+coordNeighbors,coordNeighbors.sameElements(adjacent))
	}
}