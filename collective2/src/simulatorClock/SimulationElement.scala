package simulatorClock

import scala.actors._
import Actor._

/*
case class TimeStep(time: Int)
case class FinishedTimeStep(time: Int)
case class MaxTimeStep(time: Int)
case class ConfirmedMaxTimeStep()
 */

trait SimulationElement extends Actor{
	var maxTimeStep: Int = _
	var clock: Clock = _
	
	def execute(timeStep: Int)
	
	final def exiting(time: Int){
		println(this+" reached maximum "+time+" and now exiting")
		clock ! RemoveSubscriber(this)
		exit()
	}
	
	def act(){
		loop{
			react{
				case MaxTimeStep(clock,maxTime)=>{
					this.clock = clock
					maxTimeStep = maxTime
					reply ( ConfirmedMaxTimeStep() )
				}
				case TimeStep(time: Int)=> {
					execute(time)
					reply( FinishedTimeStep(time) )
					if(time >= maxTimeStep){
						exiting(time)
					}
				}
			}//end react
		}//end loop
	}//end act

}