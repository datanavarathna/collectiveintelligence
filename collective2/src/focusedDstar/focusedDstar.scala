package focussedDstar

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

abstract class State extends Ordering[State]{
	var parent: State = null
	var tag = Tag.New
	var biasedEstimatedPathCost: Double = 0 //fB
	var estimatedPathCost: Double = 0 //f
	var k,h: Double = 0
	var agentStateWhenModified: State = null
	var neighbors: List[State] = Nil
	
}
class Goal(){
	private var empty = true
	
	def isEmpty: Boolean = true 
}

object Goal{
	def apply() = new Goal
	def NoPath = new NoPath
}

class NoPath extends Goal {
	override val isEmpty = true
}

class ReachedGoal extends Goal {
	
}

abstract class focusedDstar {
	var goal: State
	private var initialAgentState: State = null
	var biasedF: Double
	private var numOfAgentState: Int = 0
	private var agentStates = new ListBuffer[State]() //zero based
	private var bias = new WeakHashMap[State,Double]()//key State, value = Double
	val biasEpsilon = Double.Epsilon
	val noPath = 'NoPath
	
	private var open: TreeSet[State] = TreeSet.empty[State](State.ordering)
	
	
	def currentState: State = agentStates.head //Rcurr
	def currentState_=(currentState: State) = {
		if(agentStates.isEmpty)
			initialAgentState = currentState
		agentStates.prepend(currentState)
	}
	
	def sensor: Map[(State,State),Double] //sensor cost of state transversal
	
	//c(X,Y)
	//cost of moving from state X to state Y, always positive
	def costOfTransversal(x: State, y: State): Double
	def updateCostOFTransversal(x: State, y: State, costValue: Double)
	
	//h(x)
	//estimate of cost from state X to Goal, always positive
	def heuristic(x: State): Double = costOfTransversal(x,goal)
	
	//g(x,y)
	//estimated path cost from state X to state Y
	def focussingHeuristic(x: State, y: State) = costOfTransversal(x,y)
	
	//f(X,Ri)
	//estimated agent path cost
	def estimateAgentPathCost(x: State, agent: State): Double = {
		heuristic(x) + focussingHeuristic(x,agent)
	}
	
	//fB(X,Ri)
	def biasedEstimatedAgentPathCost(x: State, agent: State): Double = {
		estimateAgentPathCost(x,agent) + accruedBias(agent)
	}
	
	//d(Ri,R0)
	def accruedBias(agentState: State, initialAgentState: State = this.initialAgentState): Double = {
		if(bias.isEmpty)
			bias += (agentState -> 0.0 )
		bias.getOrElseUpdate(currentState,
			{
				focussingHeuristic(agentState,agentStates.head)+ biasEpsilon + accruedBias(agentStates.head)
				/*
				//not performance optimized
				var tempBias: Double = 0
				var agentStateIndex = agentStates.indexOf(agentState)
				var sequence = {
					if(agentStateIndex < 0){
						tempBias += focussingHeuristic(agentState,agentStates.head)+ biasEpsilon
						agentStates
					}else
						agentStates.drop(agentStateIndex)
				}
				var iterator = sequence.iterator
				var newerState: State = null
				var currentState: State = iterator.next
				while(iterator.hasNext){
					newerState = currentState
					currentState = iterator.next
					tempBias += focussingHeuristic(newerState,currentState)
				}
				tempBias += sequence.size*biasEpsilon
				return tempBias
				*/
			}
		)
	}//end accruedBias
	
	def delete(x: State) = {
		x.tag = Tag.Closed
		//remove from open
		open = open - x
	}
	
	def insert(x: State, newH: Double){
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
		x.biasedEstimatedPathCost = x.estimatedPathCost + accruedBias(currentState)
		open = open + x
	}//end insert
	
	def minState(): State = {
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
	
	def minValue(): Option[(Double,Double)] = {
		if(!open.isEmpty){
			var x: State = open.min(State.ordering)
			Some(x.estimatedPathCost, x.k )
		}else
			None
	}
	
	def modifyCost(x: State, y: State, costValue: Double): Option[(Double,Double)] = {
		updateCostOFTransversal(x,y,costValue)
		if(x.tag == Tag.Closed )
			insert(x,x.h)
		return minValue
	}
	
	def cost(x: State): (Double, Double) = {
		var guess = heuristic(x)
		var estimatedPathCost = guess + focussingHeuristic(x, currentState)
		return (estimatedPathCost, guess)
	}
	
	def lessThanTest(a: (Double, Double), b: (Double,Double)): Boolean = {
		val (a1, a2) = a
		val (b1, b2) = b
		if( a1 == b1)
			return a2 < b2
		else
			return a1 < b1	
	}
	
	def lessThanEqualTest(a: (Double, Double), b: (Double,Double)): Boolean = {
		val (a1, a2) = a
		val (b1, b2) = b
		if( a1 == b1)
			return a2 <= b2
		else
			return a1 <= b1	
	}
	
	def moveAgent(start: State, goal: State): Goal = {
		//initialize
		var accruedBias: Double = 0
		currentState = start
		insert(goal,0)
		var temp: Option[(Double,Double)] = Some(0.0,0.0)
		//find optimal path
		while(start.tag != Tag.Closed &&
				temp != None /*path exists*/){
			temp = processState()
		}
		if(start.tag == Tag.New )
			return Goal.NoPath
		var agentState = start
		while(agentState != goal){
			var discrepancies = sensor.filter( element => {
					val ((x: State, y: State), cost: Double) = element
					(cost != costOfTransversal(x,y)) 
				}
			)
			if(!discrepancies.isEmpty)//if sensor readings disagree with model
			{
				if(agentState != currentState){
					accruedBias = accruedBias + focussingHeuristic(agentState,currentState) + biasEpsilon
					currentState = agentState//update focal point
				}
				discrepancies.foreach(element => {
						val ((x: State, y: State), cost: Double) = element
						temp = modifyCost(x,y,cost)
					}
				)//end processing discrepancies
				//update costs and replan
				while( temp != None &&
						lessThanTest(temp.asInstanceOf[(Double,Double)], cost(agentState)) ){
					temp = processState() 
				}
			}//end if discrepancies exist
		}//end while state not goal
			
		return new Goal
	}
	
	def processState(): Option[(Double,Double)] = {
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
		else if(kval == x.h){
			for(y <- x.neighbors )
			{
				var c = costOfTransversal(y,x)
				var hValue = x.h + c
				if((y.tag  == Tag.New ) || 
						(y.parent == x && y.h != hValue) ||
						(y.parent != x && y.h > hValue) ){
					y.parent = x
					insert(x,hValue)
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
					insert(x,hValue)
				}else{
					if((y.parent != x && y.h > hValue) && (x.tag == Tag.Closed ) )
						insert(x,x.h)
					else
						if(lessThanTest(temp,cost(y)))
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