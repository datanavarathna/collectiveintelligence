package tests

import org.scalatest.junit.JUnitSuite
import org.junit._
import Assert._

import collective.definitions._

class predefPackageObject extends JUnitSuite{

	@Test def indexToXYTest() {
	/*	Test Array
	 * 	0	1	2
	 * 	3	4	5
	 */
		try {
			indexToXY(sizeX = 0, index = 0)
			fail("Failed to throw exception for sizeX = 0")
		}catch {
			case e => {
				//println(e)
				assertTrue(true)
			}
		}
		assert(indexToXY(sizeX = 3, index = 0) === (0,0) )
		assert(indexToXY(sizeX = 3, index = 4) === (1,1) )
	}
	
	@Test def xyToIndexTest() {
	/*	Test Array
	 * 	0	1	2
	 * 	3	4	5
	 */
	try {
			xyToIndex(x = 1, y = 1, sizeX = 0)
			fail("Failed to throw exception for sizeX = 0")
		}catch {
			case e => {
				//println(e)
				assertTrue(true)
			}
		}
	assert(xyToIndex(x = 1, y = 1, sizeX = 3) === (4) )
	}
}