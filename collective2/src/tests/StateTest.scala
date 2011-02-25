package tests

import org.scalatest.junit.JUnitSuite
import org.junit._
import Assert._

import focusedDstar._

class TestDStar extends CartesianCoordinateOneUnitDiagonalDStar{
	setStateTransitionOperation({state: State => true})
	currentState = CoordinateState(0,0,true,null)
	goal = CoordinateState(0,-1,true,null)
	def sensor: Map[(State,State),Double] = Map.empty[(State,State),Double]
	
}

class StateTest extends JUnitSuite{
	
	@Test def stateCompareTest() {
		var A = CoordinateState(0,0,true,null)
		var B = CoordinateState(1,1,true,null)
		
		A.biasedEstimatedPathCost = 1
		B.biasedEstimatedPathCost = 2
		//A: fB = 1 f = 0 k = 0
		//B: fB = 2 f = 0 k = 0
		assertTrue(A < B)
		
		A.biasedEstimatedPathCost = 2
		A.estimatedPathCost = 1
		B.estimatedPathCost = 2
		//A: fB = 2 f = 1 k = 0
		//B: fB = 2 f = 2 k = 0
		assertTrue(A < B)
		
		A.estimatedPathCost = 2
		A.k = 1
		B.k = 2
		//A: fB = 2 f = 2 k = 1
		//B: fB = 2 f = 2 k = 2
		assertTrue(A < B)
		
		A.k = 2
		//A: fB = 2 f = 2 k = 2
		//B: fB = 2 f = 2 k = 2
		assertTrue(!(A < B) && !(A > B) )
		
		B.biasedEstimatedPathCost = 1
		A.biasedEstimatedPathCost = 2
		//A: fB = 2 f = 2 k = 2
		//B: fB = 1 f = 2 k = 2
		assertTrue(A > B)
		
		B.biasedEstimatedPathCost = 2
		B.estimatedPathCost = 1
		A.estimatedPathCost = 2
		//A: fB = 2 f = 2 k = 2
		//B: fB = 2 f = 1 k = 2
		assertTrue(A > B)
		
		B.estimatedPathCost = 2
		B.k = 1
		A.k = 2
		//A: fB = 2 f = 2 k = 2
		//B: fB = 2 f = 2 k = 1
		assertTrue(A > B)
	}
	
	@Test def insertNewState(){
		val testDstar = new TestDStar
		var A = CoordinateState(0,0,true,null)
		var B = CoordinateState(1,1,true,null)
		
		testDstar.insert(B,1)
		testDstar.insert(A,0)
		var openList = testDstar.openElements
		assertTrue(openList+" != "+List(A,B),openList == List(A,B))
	}
	
	@Test def minStateState(){
		val testDstar = new TestDStar
		var A = CoordinateState(0,0,true,null)
		var B = CoordinateState(1,1,true,null)
		
		testDstar.insert(B,1)
		testDstar.insert(A,0)
		val minState = testDstar.minState
		assertTrue("Returned "+minState,minState == A)
	}
}