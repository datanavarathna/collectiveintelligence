package tests

import org.scalatest.junit.JUnitSuite
import org.junit._
import Assert._
import scala.actors._
import Actor._
import Futures.future

import timestampConcurrency._
import simulatorClock._

class Resource(name: String) extends TimeStampConcurrency{
	override def toString = "Resource ("+name+")"
}

class TimestampConcurrencyTest  extends JUnitSuite{
	def sleep(milliseconds: Long){
		future {Thread.sleep(milliseconds)}()
	}
	
	@Test def alteternativeTransactionsTest(){
		val maxRetries = 5
		
		val X = new Resource("X")
		val Y = new Resource("Y")
		val Z = new Resource("Z")
		
		val transactions = List( new Transaction("T1",maxRetries) with SimulationElement {
			def execute(timeStep: Int){
				timeStep match{
					case t @ 2 => {
						println(this+" at t="+t)
						assertTrue(Y.read(this))
					}
					case t @ 7 => {
						println(this+" at t="+t)
						assertTrue(X.read(this))
					}
					case time =>{
						if(time > 7){
							println(this+" finished job at "+time)
							
							exitAt(time)
						}else if(time < 0){
							throw new Exception(this+ "received an Invalid TimeStep")
						}
							
					}//end case time
				}//end match TimeStep
			}//end execute	
		}//end SimulationElement
		
		,new Transaction("T2",maxRetries) with SimulationElement {
			def execute(timeStep: Int){
				timeStep match{
					case t @ 1 => {
						println(this+" at t="+t)
						assertTrue(Y.read(this))
					}
					case time =>{
						if(time > 1){
							println(this+" finished job at "+time)
							exitAt(time)
						}else if(time < 0){
							throw new Exception(this+ "received an Invalid TimeStep")
						}
							
					}//end case time
				}//end match TimeStep
			}//end execute	
		}//end SimulationElement
		
		,new Transaction("T3",maxRetries) with SimulationElement {
			def execute(timeStep: Int){
				timeStep match{
					case t @ 3 => {
						println(this+" at t="+t)
						assertTrue(Y.write(this))
					}
					case t @ 4 => {
						println(this+" at t="+t)
						assertTrue(Z.write(this))
					}
					case t @ 6 => {
						println(this+" at t="+t)
						assertFalse(Z.read(this))
					}
					case time =>{
						if(time > 6){
							println(this+" finished job at "+time)
							clock ! RemoveSubscriber(this)
							exitAt(time)
						}else if(time < 0){
							throw new Exception(this+ "received an Invalid TimeStep")
						}
							
					}//end case time
				}//end match TimeStep
			}//end execute	
		}//end SimulationElement
		
		,new Transaction("T4",maxRetries) with SimulationElement {
			def execute(timeStep: Int){
				timeStep match{
					case t @ 8 => {
						println(this+" at t="+t)
						assertFalse(Z.write(this))
					}
					case time =>{
						if(time > 8){
							println(this+" finished job at "+time)
							clock ! RemoveSubscriber(this)
							exitAt(time)
						}else if(time < 0){
							throw new Exception(this+ "received an Invalid TimeStep")
						}
							
					}//end case time
				}//end match TimeStep
			}//end execute	
		}//end SimulationElement
		
		,new Transaction("T5",maxRetries) with SimulationElement {
			def execute(timeStep: Int){
				timeStep match{
					case t @ 1 => {
						println(this+" at t="+t)
						assertTrue(X.read(this))
					}
					case t @ 5 => {
						println(this+" at t="+t)
						assertTrue(Z.read(this))
					}
					case t @ 9 => {
						println(this+" at t="+t)
						assertTrue(Y.write(this))
					}
					case t @ 10 => {
						println(this+" at t="+t)
						assertTrue(Z.write(this))
					}
					case time =>{
						if(time > 10){
							println(this+" finished job at "+time)
							clock ! RemoveSubscriber(this)
							exitAt(time)
						}else if(time < 0){
							throw new Exception(this+ "received an Invalid TimeStep")
						}
							
					}//end case time
				}//end match TimeStep
			}//end execute	
		}//end SimulationElement
		
		)
		
		println(transactions)
		
		val clock = new SimClock(maxTimeStep=10)
		transactions.foreach(t => {
				clock.addSubscriber(t)
				t.start
				//sleep(1)
			}
		)

		clock.start
		var ft = {clock !! 'Start}
		ft()
		//sleep(1)
		if(clock.getState == Actor.State.New)
			fail
		transactions.foreach( t => if(t.getState == Actor.State.New) fail )
	}
	
		@Test def transactionsTest(){
		val maxRetries = 5
		
		val X = new Resource("X")
		val Y = new Resource("Y")
		val Z = new Resource("Z")
		
		val transactions = List( new Transaction("T1",maxRetries) with SimulationElement {
			def execute(timeStep: Int){
				timeStep match{
					case t @ 2 => {
						println(this+" at t="+t)
						assertTrue(Y.read(this))
					}
					case t @ 7 => {
						println(this+" at t="+t)
						assertTrue(X.read(this))
					}
					case time =>{
						if(time > 7){
							println(this+" finished job at "+time)
							
							exitAt(time)
						}else if(time < 0){
							throw new Exception(this+ "received an Invalid TimeStep")
						}
							
					}//end case time
				}//end match TimeStep
			}//end execute	
		}//end SimulationElement
		
		,new Transaction("T2",maxRetries) with SimulationElement {
			def execute(timeStep: Int){
				timeStep match{
					case t @ 1 => {
						println(this+" at t="+t)
						assertTrue(Y.read(this))
					}
					case time =>{
						if(time > 1){
							println(this+" finished job at "+time)
							exitAt(time)
						}else if(time < 0){
							throw new Exception(this+ "received an Invalid TimeStep")
						}
							
					}//end case time
				}//end match TimeStep
			}//end execute	
		}//end SimulationElement
		
		,new Transaction("T3",maxRetries) with SimulationElement {
			def execute(timeStep: Int){
				timeStep match{
					case t @ 3 => {
						println(this+" at t="+t)
						assertTrue(Y.write(this))
					}
					case t @ 4 => {
						println(this+" at t="+t)
						assertTrue(Z.write(this))
					}
					case t @ 6 => {
						println(this+" at t="+t)
						assertFalse(Y.read(this))
					}
					case time =>{
						if(time > 6){
							println(this+" finished job at "+time)
							clock ! RemoveSubscriber(this)
							exitAt(time)
						}else if(time < 0){
							throw new Exception(this+ "received an Invalid TimeStep")
						}
							
					}//end case time
				}//end match TimeStep
			}//end execute	
		}//end SimulationElement
		
		,new Transaction("T4",maxRetries) with SimulationElement {
			def execute(timeStep: Int){
				timeStep match{
					case t @ 8 => {
						println(this+" at t="+t)
						assertFalse(Z.write(this))
					}
					case time =>{
						if(time > 8){
							println(this+" finished job at "+time)
							clock ! RemoveSubscriber(this)
							exitAt(time)
						}else if(time < 0){
							throw new Exception(this+ "received an Invalid TimeStep")
						}
							
					}//end case time
				}//end match TimeStep
			}//end execute	
		}//end SimulationElement
		
		,new Transaction("T5",maxRetries) with SimulationElement {
			def execute(timeStep: Int){
				timeStep match{
					case t @ 1 => {
						println(this+" at t="+t)
						assertTrue(X.read(this))
					}
					case t @ 5 => {
						println(this+" at t="+t)
						assertTrue(Z.read(this))
					}
					case t @ 9 => {
						println(this+" at t="+t)
						assertTrue(Y.write(this))
					}
					case t @ 10 => {
						println(this+" at t="+t)
						assertTrue(Z.write(this))
					}
					case time =>{
						if(time > 10){
							println(this+" finished job at "+time)
							clock ! RemoveSubscriber(this)
							exitAt(time)
						}else if(time < 0){
							throw new Exception(this+ "received an Invalid TimeStep")
						}
							
					}//end case time
				}//end match TimeStep
			}//end execute	
		}//end SimulationElement
		
		)
		
		println(transactions)
		
		val clock = new SimClock(maxTimeStep=10)
		transactions.foreach(t => {
				clock.addSubscriber(t)
				t.start
				//sleep(1)
			}
		)

		clock.start
		var ft = {clock !! 'Start}
		ft()
		//sleep(1)
		if(clock.getState == Actor.State.New)
			fail
		transactions.foreach( t => if(t.getState == Actor.State.New) fail )
	}
}