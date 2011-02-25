package tests

import org.scalatest.junit.JUnitSuite
import org.junit._
import Assert._
import scala.actors._
import Actor._

import timestampConcurrency._
import simulatorClock._

class TimestampConcurrencyTest  extends JUnitSuite{
	@Test def transactionsTest(){
		val maxRetries = 5
		val t1= new Transaction("T1",maxRetries) with SimulationElement {
			def execute(timeStep: Int){
				timeStep match{
					case 1 =>
					case 2 => 
					case 3 =>
					case 4 =>
					case 5 =>
					case time =>{
						if(time > 5){
							//finished job
							exiting(time)
						}else{
							throw new Exception(this+ "received an Invalid TimeStep")
						}
							
					}//end case time
				}//end match TimeStep
			}//end execute
			
		}//end SiumulationElement
		
		
		
	}
	
}