package focusedDstar

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.WeakHashMap
import scala.collection.immutable.TreeSet
import scala.math

object Tag extends Enumeration{
	val New, Closed, Open = Value
}
/*
 object State extends Ordering[State]{
	val ordering: Ordering[State] = Ordering.fromLessThan[State](State.compare(_,_) < 0)
	
	
}
*/
abstract class StateConstructor(){
	def getNeighbors(state: State): Seq[State]
}

abstract class State(factory: StateConstructor) extends Ordered[State]/*extends Ordering[State]*/
{
	var parent: State = null
	var tag = Tag.New
	var biasedEstimatedPathCost: Double = 0 //fB
	var estimatedPathCost: Double = 0 //f
	var k,h: Double = 0
	var agentStateWhenModified: State = null
	
	override def toString() = "State(tag="+tag+",k="+k+",h="+h+",f="+estimatedPathCost+",fB="+biasedEstimatedPathCost+")"
	//"State(tag="+tag+",k=%5.1,f=%5.1f,fb=%5.1f),".format(k,h,estimatedPathCost,biasedEstimatedPathCost)
	
	lazy val neighbors: Seq[State] = factory.getNeighbors(this)
	/*
	private[this] var neighborsSeq: Seq[State] = initialNeighbors
	
	def neighbors = neighborsSeq
	def neighbors_(newNeighbors: Seq[State]){
		if(neighborsSeq.isEmpty)
			neighborsSeq = newNeighbors
	}*/
	
	/*
	def transitionCostTo(that: State): Double
	def updateCostTo(that: State, cost: Double)
	*/
	
	def reset(){
		parent = null
		tag = Tag.New
		biasedEstimatedPathCost = 0 //fB
		estimatedPathCost = 0 //f
		k = 0
		h = 0
		agentStateWhenModified = null
	}
	/*
	implicit def stateOrdering = new Ordering[State]{
		def comare(a: State, b: State) = a.compare(b)
	}
	*/
	
	def compare (x: State) : Int = { 
		//(-) if x < y, (+) if x > y, 0 if x == y
		var xTemp = this.biasedEstimatedPathCost
		var yTemp = x.biasedEstimatedPathCost
		if( xTemp == yTemp){
			//compare estimatedPathCost
			xTemp = this.estimatedPathCost 
			yTemp = x.estimatedPathCost
			if( xTemp == yTemp){
				//compare k
				xTemp = this.k 
				yTemp = x.k 
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

case class Goal(var isUnreachable: Boolean = false){
	private[this] var pathList: List[State] = Nil
	
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
	val obstacleCost: Double = 1E6
	val biasEpsilon: Double = Double.Epsilon
	var goal: State = _
	//private[this] var initialAgentState: State = null
	//var biasedF: Double
	//private[this] var numOfAgentState: Int = 0
	private[this] var agentStates = new ListBuffer[State]() //zero based
	//private[this] var bias = new WeakHashMap[State,Double]()//key State, value = Double
	//val noPath = 'NoPath
	var path: Goal = new Goal
	private[this] var closedList: List[State] = Nil
	
	private[this] var stateTransitionOperation: ( State => Boolean) = _
	
	private[this] var failedToTransition = 0
	
	var maxProcessNumber : Int = 20
	private[this] var processNumber: Int = 0
	
	def openElements: List[State] = open.toList
	
	def setStateTransitionOperation( transitionOperation: State => Boolean){
		//must be called before moveAgent
		stateTransitionOperation  = transitionOperation
	}
	
	private[this]  var open: TreeSet[State] = TreeSet.empty[State]//(State.ordering)
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
			failedToTransition = 0  
		}else{
			//println("FAILED to move to transition to "+next)
			//println("path= "+path)
			failedToTransition += 1 
			//throw new Exception("Failed to move to transition state")
		}
		//println("path= "+path)
		//println("CurrentState = "+currentState)
		//println("Exited stateTransitionOperation")
		currentState
	}
	
	def update(x: State){
		if(goal != null)
			x.h = heuristic(x)
		//x.estimatedPathCost = x.k + focussingHeuristic(x,currentState)
		//x.biasedEstimatedPathCost = x.estimatedPathCost + accruedBias	
	}
	
	def updateTransition(x: State, y: State, costValue: Double){
		updateCostOfTransversal(x,y,costValue)
		
		//update X
		x.estimatedPathCost = x.k + focussingHeuristic(x,currentState)
		x.biasedEstimatedPathCost = x.estimatedPathCost + accruedBias

		
		//update Y
		y.estimatedPathCost = y.k + focussingHeuristic(y,currentState)
		y.biasedEstimatedPathCost = y.estimatedPathCost + accruedBias
		//if(goal != null)
		//	y.h = heuristic(x)
		//y.k = 0
	}
	
	def updateCostOfTransversal(x: State, y: State, costValue: Double) /*= {
		x.updateCostTo(y,costValue)
	}*/
	
	//h(x)
	//estimate of cost from state X to Goal, always positive
	def heuristic(x: State): Double = {
		var costEstimate: Double = 0
		var thisState = x
		var next = x.parent
		while(next != null && thisState != goal){
			costEstimate += costOfTransversal(thisState,next)
			thisState = next
			next = thisState.parent 
		}
		if(thisState != goal)
			costEstimate += costOfTransversal(x,goal)
		costEstimate
	}
	
	/*
	//works same as x.h
	private[this] var heuristicValues = collection.mutable.WeakHashMap.empty[State,Double]
	def h(x: State):Double = heuristicValues.getOrElseUpdate(x,{
		if(goal != null)
			heuristic(x)
		else
			0.0
	})
	def h(x: State, value: Double){
		heuristicValues += (x->value)
	}
	*/
	
	//g(x,y)
	//estimated path cost from state X to state Y
	def focussingHeuristic(x: State, y: State) = costOfTransversal(x,y)
	
	protected def delete(x: State) = {
		//println("delete( "+x+" )")
		x.tag = Tag.Closed
		//remove from open
		open = open - x
		//println("open: size="+open.size+" "+open)
		closedList = x :: closedList
		//println("closed: "+closedList)
	}
	
	protected def putState(x: State){
		//println("putState( "+x+" )")
		x.tag = Tag.Open 
		open = open + x
		//println("x.tag: "+(x.tag))
		//println("open: "+open)
	}
	
	/*private*/ def insert(x: State, newH: Double){
		/*
		if(newH == 0){
			goal = x
			//update(x)
		}
		*/
		//println("insert ( "+x+", "+newH+")")
		
		if(x.tag == Tag.New )
			x.k = newH
		else if(x.tag == Tag.Open ){
			x.k = math.min(x.k,newH)
			delete(x)
		}else{
			x.k = math.min(x.h,newH)
		}
		x.h = newH
		x.agentStateWhenModified = currentState
		x.estimatedPathCost = x.k + focussingHeuristic(x,currentState)
		x.biasedEstimatedPathCost = x.estimatedPathCost + accruedBias//accruedBias(currentState)
		/*
		//println("x.h= "+x.h)
		//println("x.agentStateWhenModified= "+x.agentStateWhenModified)
		//println("x.estimatedPathCost= "+x.estimatedPathCost)
		//println("x.biasedEstimatedPathCost= "+x.biasedEstimatedPathCost)
		*/
		putState(x)
	}//end insert
	
	/*private*/ def minState(): State = {
		//println("minState")
		var result: State = null
		var foundResult = false
		while(!open.isEmpty && !foundResult){
			var x = open.min//(State.ordering)
			//update(x)
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
		//println(" = "+result)
		return result
	}
	
	protected def minValue(): Option[(Double,Double)] = {
		val result = {
				if(!open.isEmpty){
					var x: State = open.min//(State.ordering)
					Some(x.estimatedPathCost, x.k )
				}else
					None
		}
		//println("minValue = "+result)
		result
	}
	
	def modifyCost(x: State, y: State, costValue: Double): Option[(Double,Double)] = {
		//println("modifyCost( "+x+", "+y+", "+costValue+" )")
		updateTransition(x,y,costValue)
		//println("Updates( "+x+", "+y+", "+costValue+" )")
		if(x.tag == Tag.Closed ){
			//println("x.tag = Tag.Closed")
			insert(x,heuristic(x))
		}
		return minValue
	}
	
	protected def cost(x: State): (Double, Double) = {
		//println("cost( "+x+" )")
		//update(x))
		var guess= heuristic(x)
		var estimatedPathCost = guess + focussingHeuristic(x, currentState)
		x.estimatedPathCost = estimatedPathCost
		//println("= ("+estimatedPathCost+","+guess+")")
		return (estimatedPathCost, guess)
	}
	
	protected def lessThanTest(a: (Double, Double), b: (Double,Double)): Boolean = {
		val (a1, a2) = a
		val (b1, b2) = b
		if( a1 == b1)
			return a2 < b2
		else
			return a1 < b1	
	}
	
	protected def lessThanEqualTest(a: (Double, Double), b: (Double,Double)): Boolean = {
		val (a1, a2) = a
		val (b1, b2) = b
		if( a1 == b1)
			return a2 <= b2
		else
			return a1 <= b1	
	}
	
	protected def resetCheckedStates(){
		//println("resetCheckedStates")
		open = TreeSet.empty[State]//(State.ordering)
		for(state <- closedList){
			state.reset()
			state.neighbors.foreach( neighbor => neighbor.reset())
			//insert(state,state.k )//causes fail to move upon subsequent moveAgent calls
		}
		closedList = Nil
		
		//heuristicValues = collection.mutable.WeakHashMap.empty[State,Double]
	}
	
	def initialScan(){
		sensor.foreach(element => {
			val ((x: State, y: State), costValue: Double) = element
			updateTransition(x,y,costValue)
		})
	}
	
	def moveAgent(start: State, goal: State): Goal = {
		try{
			
		println("Executing moveAgent( "+start+" , "+goal+" )")
		this.goal=goal
		//println("Initialize")
		//initialize
		path = new Goal
		resetCheckedStates()
		failedToTransition = 0
		
		accruedBias = 0
		initializeCurrentState(start)//currentState = start
		insert(goal,0)
		var temp: Option[(Double,Double)] = Some(0.0,0.0)
		//println("Find optimal path")
		//find optimal path
		processNumber =0
		while(start.tag != Tag.Closed &&
				temp != None /*unobstructed path exists to goal from start*/ 
				&& stateReachable(goal)
				&& {
						if(processNumber <= maxProcessNumber)
							true
						else{
							println("Initial planning surpassed maxProcessNumber")
							false
						}
					}
		){
			processNumber += 1
			/*
			println("open: "+open)
			println("start: "+start)
			println("PROCESS STATE NUMBER "+processNumber)
			*/
			temp = processState()
			/*
			println("open: "+(open) )
			println("start: "+start)
			println("start.tag: "+(start.tag) )
			println("start.tag != Tag.Closed: "+(start.tag != Tag.Closed) )
			println("temp: "+temp)
			println("temp != None: "+(temp != None))
			println("stateReachable(goal): "+stateReachable(goal))
			*/
		}
		{
			//println("if(start.h >= obstacleCost)")
			if(start.h >= obstacleCost){
				println("NO UNOBSTRUCTED PATH EXISTS from "+start+" to "+goal)
				val result=new Goal(isUnreachable = true)
				result.addStateToPath(goal)
				return result
			}
		}
		if(start.tag == Tag.New )//goal is an unreachable state
		{
			println("Goal "+goal+" is unreachable")
			return new Goal
		}
		var agentState = start
		path.addStateToPath(start)
		while(agentState != goal && stateReachable(goal) && failedToTransition < 5){
			if(failedToTransition > 1){
				println("FAILED TRANSITION NUMBER "+failedToTransition)
			}
			//println("Check for discrepancies between sensor readings and state transistion costs")
			//check that sensor is not empty
			val sensorReadings = sensor
			if(sensorReadings == null){
				//println("Sensor was empty")
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
			if(!discrepancies.isEmpty || failedToTransition > 0)//if sensor readings disagree with model
			{
				//println("Discrepancies exist")
				if(agentState != currentState){
					//println("Update focal point of agentState")
					accruedBias = accruedBias + focussingHeuristic(agentState,currentState) + biasEpsilon
					currentState = agentState//update focal point
				}
				//println("Update state transition costs")
				discrepancies.foreach(element => {
						val ((x: State, y: State), costValue: Double) = element
						temp = modifyCost(x,y,costValue)
					}
				)//end processing discrepancies
				//update costs and replan
				//println("Replan")
				processNumber = 0
				while( temp != None /*unobstructed path exists to goal from current state*/ && {
					var Some(doubleDouble) = temp
					//println("? "+temp+" < "+cost(agentState))
					lessThanEqualTest( doubleDouble, cost(agentState))//prevents replanning, COST is probably the problem
				}&& stateReachable(goal)
				&& {
					if(processNumber <= maxProcessNumber)
						true
						else{
							println("Replanning surpassed maxProcessNumber")
							false
						}
				}
				){
					processNumber += 1
					//println("open: "+open)
					//println("PROCESS STATE NUMBER "+processNumber)
					temp = processState() 
				}
				{
					//println("if(agentState.h >= obstacleCost)")
					if(agentState.h >= obstacleCost){
						println("NO UNOBSTRUCTED PATH EXISTS from "+agentState+" to "+goal)
						val result=new Goal(isUnreachable = true)
						result.addStateToPath(goal)
						return result
					}
				}
			}//end if discrepancies exist
			else{
				//println("No discrepancies detected")
			}
			//println("AgentState before transitionToState "+agentState)	
			agentState = transitionToState(agentState.parent)
		}//end while state not goal
		//println("Goal reached")	
		}catch{
			case e: Exception => //println("Following eception thrown: "+e)
		}
		return path
	}
	
	protected def stateReachable(x: State):Boolean = {
		val neighborCosts = for(y <- x.neighbors )yield{
			costOfTransversal(y,x)
		}
		neighborCosts.find(element => element < obstacleCost) match {
			case None => {
				//println(goal+" is UNREACHABLE")
				false
			}
			case _ => true
		}
	}
	
	protected def processState(): Option[(Double,Double)] = {
		//println("Executing processState")
		//lowest pathCost removed from open
		var x = minState()
		if (x == null) return None
		//update(x)
		//println("x: "+x)
		var kval = x.k
		//println("  kval: "+kval)
		var temp = (x.estimatedPathCost , kval  )
		//println("  temp: "+temp)
		delete(x)
		//see if x.h can be reduced
		//update(x)
		if(kval < x.h )
		{
			//println("RAISE State")
			//println("(kval < x.h): ( "+kval+" < "+x.h+")")
			for(y <- x.neighbors )
			{
				//update(y)
				//println("  y: "+y)
				var c = costOfTransversal(y,x)
				//println("     c: "+c)
				if((y.tag  != Tag.New ) && lessThanEqualTest(cost(y),temp) && 
						x.h > y.h + c){
					//println("Set parent to better neighbor")
					//println("     "+x+"->"+y )
					x.parent = y
					x.h = y.h + c
				}
			}
		}
		//see if pathCost can be lowered for neighbor Y of X
		if(kval == x.h){
			//println("LOWER State")
			//println("(kval == x.h): ( "+kval+" == "+x.h+")")
			for(y <- x.neighbors )
			{
				//update(y)
				//println("  y: "+y)
				var c = costOfTransversal(x,y)
				//println("     c: "+c)
				var hValue = x.h + c
				//println("     hValue: "+hValue)
				if((y.tag  == Tag.New ) || 
						(y.parent == x && y.h != hValue) ||
						(y.parent != x && y.h > hValue) ){
					//println("Updated child cost")
					//println("     "+y+"->"+x )
					y.parent = x
					insert(y,hValue)
				}
			}
		}
		else{
			//println("RAISE State")
			for(y <- x.neighbors )
			{
				//cost changes propagate to New states
				//update(y)
				//println("  y: "+y)
				var c = costOfTransversal(x,y)
				//println("     c: "+c)
				var hValue = x.h + c
				//println("     hValue: "+hValue)
				if((y.tag  == Tag.New ) || 
						(y.parent == x && y.h != hValue) ){
					//println("Inserted a neighbor with a new cost value")
					//println("     "+y+"->"+x )
					y.parent = x
					insert(y,hValue)
				}else{
					//lower path cost for non-immediate descendants
					if((y.parent != x && y.h > hValue) && (x.tag == Tag.Closed ) ){
						//println("Inserted self as a holding action")
						insert(x,x.h)
					}
					//reduce path cost using suboptimal neighbor
					else{
						if( (y.parent != x && x.h > y.h + costOfTransversal(y,x)) && (y.tag == Tag.Closed ) &&
								lessThanTest(temp,cost(y))){
							//println("Inserted neighbor as a holding action")
							insert(y,y.h)
						}
					}
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