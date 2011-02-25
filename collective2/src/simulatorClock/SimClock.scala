package simulatorClock

import scala.actors._
import Actor._
import Futures.awaitAll

case class TimeStep(time: Int)
case class FinishedTimeStep(time: Int)
case class MaxTimeStep(clock: SimClock,maxTime: Int)
case class ConfirmedMaxTimeStep()
case class RemoveSubscriber(subscriber: SimulationElement)

class SimClock(val maxTimeStep: Int,timestepTimeout: Long= 1000000) extends Actor{
	var timeStep: Int = 0
	var subscribers: List[SimulationElement] = Nil
	
	def addSubscriber(actor: SimulationElement){
		subscribers = actor :: subscribers
	}
	
	private def deleteSubscriber(actor: SimulationElement){
		subscribers = subscribers.filterNot( _ != actor)
	}
	
	def initializeSubscribers(){
		val futures: List[Future[Any]] = for(subscriber<-subscribers) yield{
			subscriber !! MaxTimeStep(this,maxTimeStep)
		}
		val replies = awaitAll(timestepTimeout,futures: _*)
		replies.foreach(reply => if( reply != Some(ConfirmedMaxTimeStep()) ){
			println("Clock received an unexpected reply during initialization: "+reply) 
			throw new Exception("Clock received an unexpected reply during initialization: "+reply) 
		}
			
		)
	}
	
	def updateTimeStep(){
		val futures: List[Future[Any]] = for(subscriber<-subscribers) yield{
			subscriber !! TimeStep(timeStep)
		}	
		val replies = awaitAll(timestepTimeout,futures: _*)
		println(replies)
		replies.foreach(reply => if( reply != Some(FinishedTimeStep(timeStep))){
				println("Clock received an unexpected reply during updateTimeStep: "+reply) 
				throw new Exception("Clock received an unexpected reply during updateTimeStep: "+reply) 
			}
		)
		timeStep += 1	
	}
	
	final private def execute(){
		initializeSubscribers()
		loopWhile(timeStep<=maxTimeStep){
			updateTimeStep()
			reactWithin(1/*ms*/){
				case RemoveSubscriber(subscriber)=>{
					println("Deleting subscriber: "+subscriber)
					deleteSubscriber(subscriber)
				}
				case TIMEOUT => {}
				case catchAll => {
					println(this+" unexpected received "+catchAll)
					throw new Exception(this+" unexpected received "+catchAll)
				}//end catchAll
			}//end reactWithin
		}andThen{
			println(this+" starting")
		}
	}
	
	def act(){
		
		react{
			case 'Start => {
				val replyDestination = sender
				execute() andThen{
					println(this+" exiting")
					replyDestination ! 'End
					exit
				}
			}//end case 'Start
		}//end react
	}//end act
}