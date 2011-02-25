package simulatorClock

import scala.actors._
import Actor._
import Futures.awaitAll

case class TimeStep(time: Int)
case class FinishedTimeStep(time: Int)
case class MaxTimeStep(clock: Clock,maxTime: Int)
case class ConfirmedMaxTimeStep()
case class RemoveSubscriber(subscriber: SimulationElement)

class Clock(val maxTimeStep: Int,timeout: Long) extends Actor{
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
		val replies = awaitAll(timeout,futures: _*)
		replies.foreach(reply => if( reply != ConfirmedMaxTimeStep() )
			throw new Exception("Clock received an unexpected reply during initialization") 
		)
	}
	
	def updateTimeStep(){
		val futures: List[Future[Any]] = for(subscriber<-subscribers) yield{
			subscriber !! TimeStep(timeStep)
		}	
		val replies = awaitAll(timeout,futures: _*)
		replies.foreach(reply => if( reply != FinishedTimeStep(timeStep))
			throw new Exception("Clock received an unexpected reply during updateTimeStep") 
		)
		timeStep += 1	
	}
	
	def act(){
		initializeSubscribers()
		loopWhile(timeStep<maxTimeStep){
			updateTimeStep()
			reactWithin(10/*ms*/){
				case RemoveSubscriber(subscriber)=>deleteSubscriber(subscriber)
				case catchAll => throw new Exception(this+" unexpected received "+catchAll)
			}
		}//end loopWhile
		exit()
	}//end act
}