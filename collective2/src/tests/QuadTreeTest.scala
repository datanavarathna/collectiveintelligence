package tests

import org.scalatest.junit.JUnitSuite
import org.junit._
import Assert._

import collective2.QuadTree

class QuadTreeTest extends JUnitSuite{
	@Test def treeTest {
		var tree = new QuadTree[Double]
		tree.add(0, 0, 1.0)
		tree.add(1, 1, 2.0)
		tree.add(-1, 1, 3.0)
		tree.add(-1, -1, 4.0)
		assertTrue(tree.containsElementAt(0, 0))
		assertTrue(tree.containsElementAt(1, 1))
		assertTrue(tree.containsElementAt(-1, 1))
		assertTrue(tree.containsElementAt(-1,-1))
		assertFalse(tree.containsElementAt(0, 2))
		assertFalse(tree.containsElementAt(2, 2))
		assertFalse(tree.containsElementAt(0, -2))
		assertFalse(tree.containsElementAt(-1, 2))
		assertFalse(tree.containsElementAt(-2, 2))
		assert(tree(0, 0)=== Some(1.0))
		assert(tree(1, 1)=== Some(2.0))
		assert(tree(-1, 1)=== Some(3.0))
		assert(tree(-1,-1)=== Some(4.0))
		assert(tree(0, 2)=== None)
		assert(tree(2, 2)=== None)
		assert(tree(0, -2)=== None)
		assert(tree(-1, 2)=== None)
		assert(tree(-2, 2)=== None)
	}
	
	
}