package tests

import org.scalatest.junit.JUnitSuite
import org.junit._
import Assert._

import collective2.QuadBitSet

class QuadBitSetTest extends JUnitSuite{
	@Test def setTest {
		var set = QuadBitSet( (0,0),(1,1),(-1,-1),(-1,1) )
		assertTrue(set.contains(0, 0))
		assertTrue(set.contains(1, 1))
		assertTrue(set.contains(-1, 1))
		assertTrue(set.contains(-1,-1))
		assertFalse(set.contains(0, 2))
		assertFalse(set.contains(2, 2))
		assertFalse(set.contains(0, -2))
		assertFalse(set.contains(-1, 2))
		assertFalse(set.contains(-2, 2))
	}
	
	@Test def expansionTest {
		var set = new QuadBitSet
		set.add(30, 30)
		set.add(30, -30)
		set.add(-30, 30)
		set.add(-30, -30)
		set.add(33,33)
		set.add(33,-33)
		set.add(-33,33)
		set.add(-33,-33)
		assertTrue(set.contains(30,30))
		assertTrue(set.contains(30,-30))
		assertTrue(set.contains(-30,30))
		assertTrue(set.contains(-30,-30))
		assertTrue(set.contains(33,33))
		assertTrue(set.contains(33,-33))
		assertTrue(set.contains(-33,33))
		assertTrue(set.contains(-33,-33))
	}
	
	@Test def combinationTest {
		var set = QuadBitSet( (0,0),(1,1),(-1,-1),(-1,1) )
		var set2 = QuadBitSet( (10,10),(11,11),(-11,-11),(-11,11) )
		set += set2
		assertTrue(set.contains(0, 0))
		assertTrue(set.contains(1, 1))
		assertTrue(set.contains(-1, 1))
		assertTrue(set.contains(-1,-1))
		assertTrue(set.contains(10, 10))
		assertTrue(set.contains(11, 11))
		assertTrue(set.contains(-11, 11))
		assertTrue(set.contains(-11,-11))

	}
}