package tests

import org.scalatest.junit.JUnitSuite
import org.junit._
import Assert._

import uncertaintyMath.Measurement
import Measurement._

class UncertaintyMathTest extends JUnitSuite{
	
	var temp1: Measurement = _
	var temp2: Measurement = _
	var a: Double = 3
    var b: Double = 2
	
	@Test def intToMeasurement() {
		temp1 = -1
        temp2 = -1
		assert(temp1 === temp2)
	}
	
	@Test def doubleToMeasurement(){
        temp1 = -1.0
        temp2 = -1.0
        assert( temp1 === temp2)
	}
	
	@Test def equality() {
        temp1 = new Measurement(8,1)
        temp2 = new Measurement(10,1)
        assert(temp1 === temp2)
        temp1 = 3
        assert(temp1 != temp2)
        temp1 = 3.0
        assert(temp1 != temp2)
	}
	
	@Test def ln() {
		temp1 = Measurement.ln(3.0)
        temp2 = math.log(3.0) / math.log(math.E)
        assert(temp1 === temp2)
	}
	
	@Test def sqrt() {
		temp1 = Measurement.sqrt(3.0)
        temp2 = math.sqrt(3.0)
        assert(temp1 === temp2)
	}
    
	@Test def sin() {
		temp1 = Measurement.sin(3.0)
        temp2 = math.sin(3.0)
        assert(temp1 === temp2)
	}
       
    @Test def cos() {
		temp1 = Measurement.cos(3.0)
        temp2 = math.cos(3.0)
        assert(temp1 === temp2)
	}
    
    @Test def tan() {
		temp1 = Measurement.tan(3.0)
        temp2 = math.tan(3.0)
        assert(temp1 === temp2)
	}
    
    @Test def asin() {
		temp1 = Measurement.asin(0.7)
        temp2 = math.asin(0.7)
        assert(temp1 === temp2)
	}
    
    @Test def acos() {
		temp1 = Measurement.acos(0.7)
        temp2 = math.acos(0.7)
        assert(temp1 === temp2)
	}
    
    @Test def atan() {
		temp1 = Measurement.atan(3.0)
        temp2 = math.atan(3.0)
        assert(temp1 === temp2)
	}

    @Test def atan2Math() {
    	assert(math.atan2(0, 0) === 0)
    	assert(math.atan2(1, 0) === math.Pi/2)
    	assert(math.atan2(0, 1) === 0)
    	assert(math.atan2(-1, 0) === -math.Pi/2)
    	assert(math.atan2(0, -1) === math.Pi)
    }

    @Test def atan2() {
    	temp1 = Measurement.atan2(
    			new Measurement(0),new Measurement(1)
    	)
    	temp2 = math.atan2(0,1)
    	assert(temp1 === temp2)
    	temp1 = Measurement.atan2(
    			new Measurement(0),new Measurement(-1)
    	)
    	temp2 = math.atan2(0,-1)
    	assert(temp1 === temp2)
    	temp1 = Measurement.atan2(
    			new Measurement(1),new Measurement(0)
    	)
    	temp2 = math.atan2(1,0)
    	assert(temp1 === temp2)
    	temp1 = Measurement.atan2(
    			new Measurement(-1),new Measurement(0)
    	)
    	temp2 = math.atan2(-1,0)
    	assert(temp1 === temp2)
    	temp1 = Measurement.atan2(
    			new Measurement(1),new Measurement(1)
    	)
    	temp2 = math.atan2(1,1)
    	assert(temp1 === temp2)
    }    
    
    @Test def subtract() {
    	temp1 = a
        temp2 = b
        assert((temp1 - temp2) === new Measurement(a - b))
    }
        
    @Test def add() {
    	temp1 = a
        temp2 = b
        assert((temp1 + temp2) === new Measurement(a + b))
    }    
    
    @Test def multiply() {
    	temp1 = a
        temp2 = b
        assert((temp1 * temp2) === new Measurement(a * b))
    }
    
    @Test def divide() {
    	temp1 = a
        temp2 = b
        assert((temp1 / temp2) === new Measurement(a / b))
    }
    
    @Test def power() {
    	temp1 = a
        temp2 = b
        assert((temp1 ^ temp2) === new Measurement(math.pow(a,b)))
    }
    
     @Test def lessThan() {
    	temp1 = 3
        temp2 = 2
        assert(temp2 < temp1)
    	assert(!(temp2 > temp1))
    }
     
    @Test def greaterThan() {
    	temp1 = 2
        temp2 = 3
        assert(temp2 > temp1)
    	assert(!(temp2 < temp1))
    } 


      /*
      @Test def quadTree() {
    	  //Test Quad Tree

      val quadTree = new QuadTreeGateway
      if(quadTree.add(Obstacle(1, 0, 0)))
        println("Added obstacle at (0,0)")
      else
        println("Failed to register add")
      if(quadTree.contains(0,0))
        println("Correctly found (0,0)")
      else
        println("Failed")
      if(quadTree.add(Obstacle(1, 1, 1)))
        println("Added obstacle at (1,1)")
      else
        println("Failed to register add")
      if(quadTree.add(Obstacle(1, 8, 3)))
        println("Added obstacle at (8,3)")
      else
        println("Failed to register add")
      if(quadTree.add(Obstacle(1, 7, 2)))
        println("Added obstacle at (7,2)")
      else
        println("Failed to register add")
      
      if(quadTree.contains(1,1))
        println("Correctly found (1,1)")
      else
        println("Failed")
      if(quadTree.contains(8,3))
        println("Correctly found (8,3)")
      else
        println("Failed")
      if(quadTree.contains(7,2))
        println("Correctly found (7,2)")
      else
        println("Failed to find (7,2)")
      if(!quadTree.contains(2,2))
        println("Correctly failed to find an obstacle at (2,2)")
      else
        println("Failed")
      if(quadTree.range(2,0,0)==List(Obstacle(1,0,0),Obstacle(1,1,1)))
        println("Correctly found Obstacles with radius 2 of 0,0")
      else
        println("Failed")
      //println("Should only contain 0,0 and 1,1")
      //println(quadTree.range(2,0,0))
      if(quadTree.range(9,0,0)==List(Obstacle(1,0,0),Obstacle(1,1,1),Obstacle(1, 8, 3),Obstacle(1, 7, 2)))
        println("Correctly found Obstacles with radius 9 of 0,0")
      else
        println("Failed")
      if(quadTree.range(4,3,3)==List(Obstacle(1,0,0),Obstacle(1,1,1),Obstacle(1, 7, 2)))
        println("Correctly found Obstacles with radius 4 of 3,3")
      else
        println("Failed" + quadTree.range(4,3,3))
      if(quadTree.range(1,3,3)==List())
        println("Correctly found no Obstacles within radius 1 of 3,3")
      else
        println("Failed")
      //passed all tests
      }
      */
   
}

object UncertaintyMathTest {
	def main(args : Array[String]) = 
	{
		println(new Measurement(2.0))
	}
}