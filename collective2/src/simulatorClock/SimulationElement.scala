package simulatorClock

import scala.actors._
import Actor._

trait SimulationElement extends Actor{
	var maxTimeStep: Int = _
	var clock: SimClock = _
	
	def execute(timeStep: Int)
	
	final private def exiting(time: Int){
		exitAt(time)
		exit()
	}
	
	final def exitAt(time: Int){
		println(this+" reached maximum "+time+" and now exiting")
		clock ! RemoveSubscriber(this)
	}
	
	def act(){
		println(this+" started")
		loop{
			react{
				case MaxTimeStep(clock,maxTime)=>{
					println(this+" initialized")
					this.clock = clock
					maxTimeStep = maxTime
					reply ( ConfirmedMaxTimeStep() )
				}
				case TimeStep(time: Int)=> {
					println(this+" at timestep "+time )
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