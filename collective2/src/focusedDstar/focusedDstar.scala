package focusedDstar

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.WeakHashMap
import scala.collection.immutable.TreeSet
import scala.math

object Tag extends Enumeration{
	val New, Closed, Open = Value
}

object State {
	val ordering: Ordering[State] = Ordering.fromLessThan[State](State.compare(_,_) < 0)
	
	def compare (x: State, y: State) : Int = { 
		//(-) if x < y, (+) if x > y, 0 if x == y
		var xTemp = x.biasedEstimatedPathCost
		var yTemp = y.biasedEstimatedPathCost
		if( xTemp == yTemp){
			//compare estimatedPathCost
			xTemp = x.estimatedPathCost 
			yTemp = y.estimatedPathCost
			if( xTemp == yTemp){
				//compare k
				xTemp = x.k 
				yTemp = y.k 
				if( xTemp == yTemp)
					return 0
				else if ( xTemp < yTemp)
					return -1
				else
					return 1
			}//end compare k
			else if ( xTemp < yTemp)
				return -1
			else
				return 1
		}//end compare estimatedPathCost
		else if ( xTemp < yTemp)
			return -1
		else
			return 1
	}//end compare
}

abstract class StateConstructor(){
	def getNeighbors(state: State): Seq[State]
}

abstract class State(factory: StateConstructor) /*extends Ordering[State]*/
{
	var parent: State = null
	var tag = Tag.New
	var biasedEstimatedPathCost: Double = 0 //fB
	var estimatedPathCost: Double = 0 //f
	var k,h: Double = 0
	var agentStateWhenModified: State = null
	
	lazy val neighbors: Seq[State] = factory.getNeighbors(this)
	/*
	private var neighborsSeq: Seq[State] = initialNeighbors
	
	def neighbors = neighborsSeq
	def neighbors_(newNeighbors: Seq[State]){
		if(neighborsSeq.isEmpty)
			neighborsSeq = newNeighbors
	}*/
	
	/*
	def transitionCostTo(that: State): Double
	def updateCostTo(that: State, cost: Double)
	*/
}

case class Goal(){
	private var pathList: List[State] = Nil
	
	def path = pathList
	def addStateToPath(state: State){
		pathList = state :: pathList
	}
	
	def isEmpty: Boolean = pathList.isEmpty
	def size: Int = pathList.size
	
	override def equals(other: Any): Boolean = {
		other match{
			//case that: NoPath => false
			case that: Goal => {
				val thatPath = that.path
				(pathList.size == thatPath.size) && 
					(pathList.sameElements(thatPath))
			}
			case _ => false
		}
	}
	
	override def toString = path.toString
}
/*
object Goal{
	//def apply() = new Goal
	def NoPath = new NoPath
}
*/
/*
case class NoPath() {
	 val isEmpty = true
	
	override def equals(other: Any): Boolean = {
		other match{
			case that: NoPath => true
			case _ => false
		}
	}
}
*/
class ReachedGoal extends Goal {
	
}

trait focusedDstar {
	val biasEpsilon: Double = Double.Epsilon
	var goal: State = _
	//private var initialAgentState: State = null
	//var biasedF: Double
	//private var numOfAgentState: Int = 0
	private var agentStates = new ListBuffer[State]() //zero based
	//private var bias = new WeakHashMap[State,Double]()//key State, value = Double
	//val noPath = 'NoPath
	var path: Goal = new Goal
	private[this] var closedList: List[State] = Nil
	
	private[this] var stateTransitionOperation: ( State => Boolean) = _
	
	def setStateTransitionOperation( transitionOperation: State => Boolean){
		//must be called before moveAgent
		stateTransitionOperation  = transitionOperation
	}
	
	private var open: TreeSet[State] = TreeSet.empty[State](State.ordering)
	var accruedBias: Double = _
	
	def currentState: State = agentStates.head //Rcurr
	def currentState_=(currentState: State) = {
		//println("Set currentState to "+currentState)
		/*
		if(agentStates.isEmpty)
			initialAgentState = currentState
		*/
		agentStates.prepend(currentState)
	}
	def initializeCurrentState(state: State) = {
		agentStates = new ListBuffer[State]()
		currentState = state
	}
	
	def sensor: Map[(State,State),Double] //sensor cost of state transversal
	
	//c(X,Y)
	//cost of moving from state X to state Y, always positive
	def costOfTransversal(x: State, y: State): Double //= x transitionCostTo y
	
	def transitionToState(next: State): State = {
		//println("Executing transitionToState")
		//println("CurrentState = "+currentState)
		if( stateTransitionOperation(next) ){
			currentState = next//successfully transitioned to the next state
			path.addStateToPath(currentState)
		}else{
			println("Failed to move to transition to "+next)
		}
		println("path= "+path)
		//println("CurrentState = "+currentState)
		//println("Exited stateTransitionOperation")
		currentState
	}
	
	def updateCostOfTransversal(x: State, y: State, costValue: Double) /*= {
		x.updateCostTo(y,costValue)
	}*/
	
	//h(x)
	//estimate of cost from state X to Goal, always positive
	def heuristic(x: State): Double = costOfTransversal(x,goal)
	
	//g(x,y)
	//estimated path cost from state X to state Y
	def focussingHeuristic(x: State, y: State) = costOfTransversal(x,y)
	
	private def delete(x: State) = {
		x.tag = Tag.Closed
		//remove from open
		open = open - x
		closedList = x :: closedList
	}
	
	private def putState(x: State){
		x.tag = Tag.Open 
		open = open + x
	}
	
	private def insert(x: State, newH: Double){
		if(newH == 0)
			goal = x
		if(x.tag == Tag.New )
			x.k = newH
		else if(x.tag == Tag.Open ){
			x.k = math.min(x.k,newH)
			delete(x)
		}else
			x.k = math.min(x.h,newH)
		x.h = newH
		x.agentStateWhenModified = currentState
		x.estimatedPathCost = x.k + focussingHeuristic(x,currentState)
		x.biasedEstimatedPathCost = x.estimatedPathCost + accruedBias//accruedBias(currentState)
		putState(x)
	}//end insert
	
	private def minState(): State = {
		var result: State = null
		var foundResult = false
		while(!open.isEmpty && !foundResult){
			var x = open.min(State.ordering)
			if(x.agentStateWhenModified != currentState){
				var newH = x.h 
				x.h = x.k 
				delete(x)
				insert(x,newH)
			}else{
				result = x
				foundResult = true
			}
		}
		return result
	}
	
	private def minValue(): Option[(Double,Double)] = {
		if(!open.isEmpty){
			var x: State = open.min(State.ordering)
			Some(x.estimatedPathCost, x.k )
		}else
			None
	}
	
	private def modifyCost(x: State, y: State, costValue: Double): Option[(Double,Double)] = {
		updateCostOfTransversal(x,y,costValue)
		if(x.tag == Tag.Closed )
			insert(x,x.h)
		return minValue
	}
	
	private def cost(x: State): (Double, Double) = {
		var guess = heuristic(x)
		x.h = guess//not sure if correct
		var estimatedPathCost = guess + focussingHeuristic(x, currentState)
		return (estimatedPathCost, guess)
	}
	
	private def lessThanTest(a: (Double, Double), b: (Double,Double)): Boolean = {
		val (a1, a2) = a
		val (b1, b2) = b
		if( a1 == b1)
			return a2 < b2
		else
			return a1 < b1	
	}
	
	private def lessThanEqualTest(a: (Double, Double), b: (Double,Double)): Boolean = {
		val (a1, a2) = a
		val (b1, b2) = b
		if( a1 == b1)
			return a2 <= b2
		else
			return a1 <= b1	
	}
	
	private def resetCheckedStates(){
		open = TreeSet.empty[State](State.ordering)
		for(state <- closedList){
			state.tag = Tag.New 
		}
		closedList = Nil
	}
	
	def moveAgent(start: State, goal: State): Goal = {
		println("Executing moveAgent( "+start+" , "+goal+" )")
		//println("Initialize")
		//initialize
		path = new Goal
		resetCheckedStates()
		
		accruedBias = 0
		initializeCurrentState(start)//currentState = start
		insert(goal,0)
		var temp: Option[(Double,Double)] = Some(0.0,0.0)
		//println("Find optimal path")
		//find optimal path
		while(start.tag != Tag.Closed &&
				temp != None /*unobstructed path exists to goal from start*/){
			temp = processState()
		}
		if(start.tag == Tag.New )//goal is an unreachable state
		{
			println("Goal is unreachable")
			return new Goal
		}
		var agentState = start
		path.addStateToPath(start)
		while(agentState != goal){
			//println("Check for discrepancies between sensor readings and state transistion costs")
			//check that sensor is not empty
			val sensorReadings = sensor
			if(sensor == null){
				println("Sensor was empty")
				throw new Exception("Sensor was empty")
			}
			//println("SensorReadings: "+sensorReadings)
			var discrepancies = sensorReadings.filter( element => {
					val ((x: State, y: State), cost: Double) = element
					(cost != costOfTransversal(x,y)) 
				}
			)
			//println("Sensor readings have been filtered")
			//println("Discrepancies: "+discrepancies)
			if(!discrepancies.isEmpty)//if sensor readings disagree with model
			{
				//println("Discrepancies exist")
				if(agentState != currentState){
					//println("Update focal point of agentState")
					accruedBias = accruedBias + focussingHeuristic(agentState,currentState) + biasEpsilon
					currentState = agentState//update focal point
				}
				//println("Update state transition costs")
				discrepancies.foreach(element => {
						val ((x: State, y: State), cost: Double) = element
						temp = modifyCost(x,y,cost)
					}
				)//end processing discrepancies
				//update costs and replan
				//println("Replan")
				while( temp != None /*unobstructed path exists to goal from current state*/ && {
							var Some(doubleDouble) = temp
							lessThanTest(doubleDouble, cost(agentState))
						}
				){
					temp = processState() 
				}
			}//end if discrepancies exist
			else
				//println("No discrepancies detected")
			//println("AgentState before transitionToState "+agentState)	
			agentState = transitionToState(agentState.parent)
		}//end while state not goal
		//println("Returning path")	
		return path
	}
	
	private def processState(): Option[(Double,Double)] = {
		//println("Executing processState")
		//lowest pathCost removed from open
		var x = minState()
		if (x == null) return None
		var kval = x.k
		var temp = (x.estimatedPathCost , kval  )
		delete(x)
		//see if x.h can be reduced
		if(kval < x.h )
		{
			for(y <- x.neighbors )
			{
				var c = costOfTransversal(y,x)
				if((y.tag  != Tag.New ) && lessThanEqualTest(cost(y),temp) && 
						x.h > y.h + c){
					x.parent = y
					x.h = y.h + c
				}
			}
		}
		//see if pathCost can be lowered for neighbor Y of X
		if(kval == x.h){
			for(y <- x.neighbors )
			{
				var c = costOfTransversal(y,x)
				var hValue = x.h + c
				if((y.tag  == Tag.New ) || 
						(y.parent == x && y.h != hValue) ||
						(y.parent != x && y.h > hValue) ){
					y.parent = x
					insert(y,hValue)
				}
			}
		}
		//cost changes propagate to New states
		else{
			for(y <- x.neighbors )
			{
				var c = costOfTransversal(y,x)
				var hValue = x.h + c
				if((y.tag  == Tag.New ) || 
						(y.parent == x && y.h != hValue) ){
					y.parent = x
					insert(y,hValue)
				}else{
					if((y.parent != x && y.h > hValue) && (x.tag == Tag.Closed ) )
						insert(x,x.h)
					else
						if( (y.parent != x && x.h > y.h + c) && (x.tag == Tag.Closed ) &&
								lessThanTest(temp,cost(y)))
							insert(y,y.h)
				}
			}
		}
		return minValue()
	}
}
/*
object scalaApp{
	def main(args : Array[String]) = 
	{
		
		
	}
}*/