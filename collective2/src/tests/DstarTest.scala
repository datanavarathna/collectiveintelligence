package tests

import org.scalatest.junit.JUnitSuite
import org.junit._
import Assert._

import focusedDstar._
import scala.collection.mutable

import scala.actors.Actor
import scala.actors.Actor._

class Cartesian1DiagonalTest(var factory: CoordinateCreator) extends 
	CartesianCoordinateOneUnitDiagonalDStar
{
	setStateTransitionOperation({next: State => true})
	
	def sensor: Map[(State,State),Double] = {
		currentState match {
			case state @ CoordinateState(x,y,_,_) => {
				var tempMap = mutable.Map.empty[(State,State),Double]
				for (horiz <- (x-1) to (x+1);vert <- (y-1) to (y+1);
					if(!(horiz == x && vert == y ) && factory.withinBounds(horiz,vert) ) ) 
					{
						//println("Getting neighbor horiz= "+horiz+" vert= "+vert)	
						tempMap += ( (state,factory.getCoordinate(horiz,vert) ) -> 1)
					}
				return tempMap.toMap[(State,State),Double]
			}
			
			case state => {
				println(state+" is an incompatable State")
				return null
			}
		}
		
	}//end sensor
	
}

class DstarTest extends JUnitSuite{
	var factory = new CoordinateCreator((0,0),(2,2))
	var dStar = new Cartesian1DiagonalTest(factory)
	var start: CoordinateState = null
	var goal: CoordinateState = null
	
	@Test(timeout = 2000) def noInfiniteRecursion {
		start = factory.createCoordinate(0, 0)
		goal = factory.createCoordinate(2, 2)
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

	@Test def pathFindingNoObstacles{
		factory = new CoordinateCreator((0,0),(2,2))
		dStar = new Cartesian1DiagonalTest(factory)
		start = factory.getCoordinate(0, 0)
		goal = factory.getCoordinate(2, 2)
		val plannedPath = dStar.moveAgent(start, goal)
		val expectedPath = new Goal
		expectedPath.addStateToPath(factory.getCoordinate(0, 0))
		expectedPath.addStateToPath(factory.getCoordinate(1, 1))
		expectedPath.addStateToPath(factory.getCoordinate(2, 2))
		val emptyGoal = new Goal
		assertTrue("Planned path was not a path",plannedPath != emptyGoal)
		assertTrue(plannedPath+" != "+expectedPath, plannedPath == expectedPath)
	}
}