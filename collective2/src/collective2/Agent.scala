package collective2

import scala.actors._
import Actor._
import uncertaintyMath.Measurement
import uncertaintyMath.Measurement._
import collective2.definitions._
import focusedDstar._
import timestampConcurrency._

class Agent(val environment: Actor, val collectiveMap: Actor,
            val sensorRange: Double, val sensorDeltaAngle: Double, val sensorDeltaRange: Double,
            override val obstacleCost: Double = 1E6, val maxTransactionAttempts: Int = 20) 
            extends  Actor with CartesianCoordinateOneUnitDiagonalDStar
{
	private[this] val waitTime: Long = 100//ms
	
	setStateTransitionOperation(moveTo)//must execute before moveAgent called
	private[this] var exploredArea = new QuadBitSet
	private[this] var obstacles = new QuadTree[Int]//Stores obstacle type at obstacle location
	
	private[this] var exploreMode = true
	private[this] var scanNumber: Int = 0
	private[this] val mainActor = self
	/*
	private var relativeLocationX: Measurement = new Measurement(0.00000001,0)
	private var relativeLocationY: Measurement = new Measurement(0.00000001,0)
	*/
	
	//private[this] var worldWidth: Int = _
	//private[this] var worldHeight: Int = _
	
	
	import scala.util.Random
	private val randomGenerator: Random = new Random
	def randomPositiveNegative1(): Int = {
      return randomGenerator.nextInt(3) - 1
	}
	
	private var lastSensorReadings: Map[(State,State),Double] = _
	
	private var stateFactory: CoordinateCreator = _
	
	def worldDimensions {
    	  println("Getting world dimensions")
    	  val worldDimensionsFuture = (environment !! 'Dimensions)
    	  println("Got worldDimensionsFuture")
    	  val (x: Int,y: Int) = worldDimensionsFuture()
    	  stateFactory = new CoordinateCreator((-1*x,-1*y),(x,y))
    	  println("Created stateFactory")
	}
	
	
	private[this] var goalIncrement = sensorRange.toInt
	private[this] var goalXAdjustment = 0
	private[this] var goalYAdjustment = 0
	private[this] var adjustmentCycle = 0
	private[this] var explorationCycle = 1
	
	def adjustGoal{
		adjustmentCycle match{
			case 0 => {
				adjustmentCycle = 1
				goalXAdjustment = explorationCycle * goalIncrement
				goalYAdjustment = 0
			}
			case 1 => {
				adjustmentCycle = 2
				goalXAdjustment = 0
				goalYAdjustment = -explorationCycle * goalIncrement
			}
			case 2 => {
				adjustmentCycle = 3
				goalXAdjustment = -explorationCycle * goalIncrement
				goalYAdjustment = 0
			}
			case 3 => {
				adjustmentCycle = 0
				goalXAdjustment = 0
				goalYAdjustment = explorationCycle * goalIncrement
				explorationCycle += 1
			}
		}
	}
	
	def changeGoal(){
		currentState match{
			case CoordinateState(x,y,_,_) => {
				var index = 0
				var goal: CoordinateState = null
				do{
					adjustGoal
					index += 1
					goal = stateFactory.getCoordinate(x+goalXAdjustment, y+goalYAdjustment)
				}while(goal == null && index <4)
				if(exploreMode && index <4){
					explore(currentState,goal)
				}else
					println("Finished Exploring")
			}
			case state => {
				throwException(state+" is an incompatable State")
			}
		}//end currentState match
	}
	
	def resetExplore(){
		explorationCycle = 1
		adjustmentCycle = 0
	}
	
	def explore(start: State, goal: State){
		println("Exploring to "+goal)
		//scan
		println("Executing moveAgent(currentState,goal)")
		//println("Obstacle: "+stateFactory.getCoordinate(0, -1))
		moveAgent(start,goal) match {
			/*
			case path: NoPath => {
				println("Pathfinding returned NoPath")
				changeGoal
				
			}
			*/
			case path: Goal => {
				//(path.size <= 1)
				println("Pathfinding returned a path")
				changeGoal
				
			}//end case path
			case other => {
				throwException(other+" returned by moveAgent instead of a Goal")
			}
		}//end match moveAgent
		
	}//end explore(start,goal)
	
	def explored(x: Int, y: Int): Boolean = {
		exploredArea.contains(x,y)
	}
	
	//agents are detected from sensor readings
	//obstacles are obtained from a (x,y) indexed data structure
	
	//scan
	//obtain possible matches from collectiveMap
	//explore to eliminate matches
	//add detected elements to map
	//navigate, then return to scan
	
	def moveTo(next: State): Boolean = {
		println("moveTo "+next)
		next match{
			case state: CoordinateState => {
				currentState match{
					case CoordinateState(x,y,_,_) => {
						if( move(state.x -x,state.y-y) )
							return true
						else{//hit an object/edge of map
							modifyCost(currentState, next, obstacleCost)
							return false
						}
					}
					case state => {
						throwException(state+" is an incompatable State")
					}
				}//end currentState match
			}//end case state: CoordinateState
			case state => throwException(state+" passed into moveTo is not a CoordinateState")
		}
	}
	
	def move(x: Int,y: Int): Boolean = {
		//scan
		Thread.sleep(waitTime)
		println("Sending move command")
		val Displacement( deltaX, deltaY) = (environment !! MoveCommand(this,x,y))()
		println("Moved ("+deltaX+","+deltaY+")")
		(deltaX != 0) || (deltaY != 0)//moved if true
	}
	
	def sensor: Map[(State,State),Double] = scan
	
	def processDetectedObstacles(detectedObstacles: List[ObjectReading]): Iterable[((Int,Int),Int)] = {
		return for(obstacle <- detectedObstacles) yield{
			 (PolarToCartesian(obstacle.angle,obstacle.distance).toIntInt,obstacle.obstacleType)	
		}
	}
	
	def processDetectedAgents(detectedAgents: List[AgentReading]): Iterable[(Int,Int)] = {
		return for(agent <- detectedAgents) yield{
			 PolarToCartesian(agent.angle,agent.distance).toIntInt	
		}
	}
	private[this]var newObstacles: List[(Int,Int,Int)]= Nil
	def createCollectiveMapSensorReadings(): List[ScannedObstacle] = {
		var result: List[(Int,Displacement,Int)] = Nil
		val obstacleList = obstacles.toList
		for(newObstacle <- newObstacles)yield
		{
			var (newObstacleX,newObstacleY,newObstacleType) = newObstacle
			ScannedObstacle(newObstacleX,newObstacleY,newObstacleType,
				{
					var relations: List[(Displacement,Int)]=Nil
					for(obstacle <- obstacleList){
						var ((obstacleX,obstacleY),obstacleType) = obstacle
						//prevent relationship with self
						if(!(obstacleX == newObstacleX && obstacleY == newObstacleY &&
							newObstacleType == obstacleType)){
								relations = (new Displacement(newObstacleX-obstacleX,
									newObstacleY-obstacleY),obstacleType)::relations
						}//end if self	
					}//end for
					relations
				}
			)//end ScannedObstacle	
		}//end for newObstacles
	}
	
	def testPotentialCollectiveMapMatches(potentialMatches: List[PotentialMatch]): (Boolean,Option[PotentialMatch]) = {
		var successful = true
		val remainingMatches = potentialMatches.filter( potentialMatch => 
			{
				val PotentialMatch(x, y, mapObstacle) = potentialMatch
				var possible = true
				var relationVectors = mapObstacle.relationsVectors
				while(possible){
					var vector = relationVectors.head
					relationVectors = relationVectors.tail
					val relationX = x + vector.x.value.toInt
					val relationY = y + vector.y.value.toInt
					var firstIteration = true
					resetExplore()
					while(!explored(relationX,relationY) ){
						//move until explored((relationX,relationY)==true
						if(firstIteration)
							firstIteration = false
						else
							changeGoal
						moveAgent(currentState,stateFactory.getCoordinate(relationX, relationY))
					}
					//if relation not seen, not match
					if(!obstacles.containsElementAt(x,y))
						possible = false
					val relation: Displacement = vector + new Displacement(x,y)
					if( obstacles.containsElementAt(x,y) &&
						!mapObstacle.possibleMatchTest( List( (relation, obstacles(x,y).asInstanceOf[Int]) ) ) ){
						possible = false
					}
				}//end while possible
				possible
			}
		)//end filter potentialMatches
		if(remainingMatches.size > 1){
			println("Several possiblilities remain")
			(false,None)
		}else{
			if(!remainingMatches.isEmpty)
				(true,Some(remainingMatches.head))
			else
				(true,None)
		}
	}
	private[this] var collectiveObstacles = new QuadTree[CollectiveObstacle]
	
	def createNewCollectiveObstacle(val obstacleType: Int): (Int,CollectiveObstacle){
		var obstacleName  = randomGenerator.nextInt
		var collectiveObstacle: Option(CollectiveObstacle) = {
			(collectiveMap !! GetCollectiveObstacle(obstacleName)()
		}
		while(collectiveObstacle != None){
			obstacleName  = randomGenerator.nextInt
			collectiveObstacle: Option(CollectiveObstacle) = {
				(collectiveMap !! GetCollectiveObstacle(obstacleName)()
			}
		}
		val sensorArea: List[Coordinate] = {
			exploredArea.XYs.map(xy => {
					val (x,y) = xy
					Displacement(x,y)
				}
			)
		}
		(obstacleName,new CollectiveObstacle(obstacleType,sensorArea) )
	}
	
	def createCollectiveObstacles(scannedObstacles: List[ScannedObstacle]){
		var obstacleCoordinates = new QuadBitSet
		for(scannedObstacle <- scannedObstacles){
			val ScannedObstacle(scannedX,scannedY,scannedType,scannedRelations) = scannedObstacle
			val scannedVectors: List[Displacement] = scannedRelations.map( scannedRelation => scannedRelation._1)
			obstacleCoordinates.add(scannedX,scannedY)
			scannedVectors.foreach( vector => {
					val Displacement(x,y) = vector
					obstacleCoordinates.add(scannedX+x.value.toInt,scannedY+y.value.toInt)
				}
			)//end foreach scannedVectors
		}//end for scannedObstacles
	}
	
	def submitDataToCollectiveMap(remainingMatch: Option[PotentialMatch],
			scannedObstacles: List[ScannedObstacle]): Boolean = {
		
		for(scannedObstacle <- scannedObstacles){
			//scannedObstacle is really an unsubmitted object
			//ScannedObstacle(x: Int,y: Int,obstacleType: Int,scannedRelations: List[(Displacement,Int)])
			/*CollectiveObstacle(val obstacleType: Int,
				private var relations: TreeMap[Displacement,CollectiveObstacle],
				sensorArea: List[Coordinate])
			*/
			val ScannedObstacle(scannnedX,scannedY,scannedType,scannedRelations) = scannedObstacle
			remainingMatch match {
				case Some(PotentialMatch(x, y, mapObstacle: CollectiveObstacle)) =>{
					//create new CollectiveObstacles, except mapObstacle
				//match them with the appropriate Displacements in Seq[(Displacement,CollectiveObstacle]
					val scannedVectors: List[Displacement] = scannedRelations.map( scannedRelation => scannedRelation._1 )
					val newObstacleRelations = mapObstacle.relationsVectors.diff(scannedVectors)
					
				//mapObstacle.addRelations(list)
				}
				case None => {
					//create new CollectiveObstacles, except mapObstacle
				//match them with the appropriate Displacements in Seq[(Displacement,CollectiveObstacle]
				//mapObstacle.addRelations(list)//send a message instead
				}
				case error => throwException("Expected PotentialMatch but received"+error)
			}	
		}
		false//change to reflect actual success
	}
	
	def expandCollectiveMap(){
		val transaction = new Transaction("Scan"+scanNumber+" Transaction",maxTransactionAttempts)
				transaction.setOperations(
					{
						val scannedObstacles = createCollectiveMapSensorReadings
						var possibleResultsFuture = (collectiveMap !! 
							GetPossibleStates(transaction,scannedObstacles) )
						possibleResultsFuture() match {
							case OperationResult(true,pMatches) => {
								var potentialMatches = pMatches.asInstanceOf[List[PotentialMatch]]
								val (successful,remainingMatch) =testPotentialCollectiveMapMatches(potentialMatches)
								if(successful)
									submitDataToCollectiveMap(remainingMatch,scannedObstacles)
								else
									return false
							}
							case OperationResult(false,_) => {false}
							case error => throwException("Transaction "+transaction.name+
														" expected OperationResult but "+
														"received CollectiveMap reply of "+error)
						}//end match possibleResultsFuture()
					}
				)//end setOperations
	}
	
	def scan(): Map[(State,State),Double] = {
		println("Scanning")
		val scanFuture = environment !! UpdateSensor(this, sensorRange, sensorDeltaAngle, sensorDeltaRange)
		val Scan(scannedArea,detectedObstacles,detectedAgents) = scanFuture()
		println("Updating explored area")
		exploredArea += scannedArea
		
		import scala.collection.mutable
		var tempMap = mutable.Map.empty[(State,State),Double]
		//println("Getting currentState")
		currentState match {
			case state @ CoordinateState(x,y,_,_) => {
				//add costs to all areas scanned, assuming empty
				//println("Adding all costs as passable transitions")
				//println("scannedArea: "+scannedArea.XYs)
				var scannedStates: List[(Int,Int)] = for(xy <- scannedArea.XYs)yield {
					val (relativeX,relativeY) = xy
					val horiz = x + relativeX
					val vert = y + relativeY
					(horiz,vert)
				}
				var scannedStatesIterator = scannedStates.toIterator
				while(scannedStatesIterator.hasNext)
				{
					val (horiz,vert) = scannedStatesIterator.next
					for(xy <- scannedStates; if{
							val (horiz2: Int,vert2: Int) = xy
							((horiz,vert) != xy ) && math.abs(horiz-horiz2)<=1 && math.abs(vert-vert2)<=1
						} ){
						val (horiz2: Int,vert2: Int) = xy
						val cost = math.max(math.abs(horiz2-horiz), math.abs(vert2-vert))
						val state1 = stateFactory.getCoordinate(horiz,vert)
						val state2 = stateFactory.getCoordinate(horiz2,vert2)
						if(state1 != null && state2 != null)
							tempMap.put((state1,state2), cost)
						//tempMap.put((stateFactory.getCoordinate(horiz2,vert2),stateFactory.getCoordinate(horiz,vert)), cost)
					}
				}
				/*
				scannedArea.XYs().foreach(
						xy => {
							val (relativeX,relativeY) = xy
							val horiz = x + relativeX
							val vert = y + relativeY
							val cost = math.max(math.abs(x-horiz), math.abs(y-vert))
							tempMap.put((state,stateFactory.getCoordinate(horiz,vert)), cost)
						}
				)//end scannedArea
				*/
				//updating cost for areas with obstalces detected
				//println("Updating costs for detected obstacles")
				processDetectedObstacles(detectedObstacles).foreach( 
						xy => {
							val ((relativeX,relativeY),obstacleType) = xy
							val horiz = x + relativeX
							val vert = y + relativeY
							obstacles.add(horiz,vert,obstacleType)
							newObstacles = (horiz,vert,obstacleType)::newObstacles
							for(xy <- scannedStates; if{
								val (horiz2: Int,vert2: Int) = xy
								((horiz,vert) != xy ) && math.abs(horiz-horiz2)<=1 && math.abs(vert-vert2)<=1
							} ){
								val (horiz2,vert2) = xy
								//println("("+horiz+","+vert+")->("+horiz2+","+vert2+") obstacle")
								val state1 = stateFactory.getCoordinate(horiz,vert)
								val state2 = stateFactory.getCoordinate(horiz2,vert2)
								//println("state1: "+state1+ " state2: "+state2)
								if(state1 != null && state2 != null){
									tempMap.put((state1,state2), obstacleCost)
									tempMap.put((state2,state1), obstacleCost)
								}
							}
						}
				)//end processDetectedObstacles
				
				//println("processing detected agents")
				processDetectedAgents(detectedAgents).foreach( 
						xy => {
							val (relativeX,relativeY) = xy
							val horiz = x + relativeX
							val vert = y + relativeY
							for(xy <- scannedStates; if{
								val (horiz2: Int,vert2: Int) = xy
								((horiz,vert) != xy ) && math.abs(horiz-horiz2)<=1 && math.abs(vert-vert2)<=1
							} ){
								val (horiz2,vert2) = xy
								//println("("+horiz+","+vert+")->("+horiz2+","+vert2+") agent")
								val state1 = stateFactory.getCoordinate(horiz,vert)
								val state2 = stateFactory.getCoordinate(horiz2,vert2)
								//println("state1: "+state1+ " state2: "+state2)
								if(state1 != null && state2 != null){
									tempMap.put((state1,state2), obstacleCost)
									tempMap.put((state2,state1), obstacleCost)
								}
							}
						}
				)//end processDetectedAgents
				//println("Updating lastSensorReadings")
				lastSensorReadings = tempMap.toMap[(State,State),Double]
				//println("lastSensorReadings = "+lastSensorReadings)
			}//end case state

			case state => {
				throwException(state+" is an incompatable State")
			}
		}//end currentState match
		println("Finished processing scan")
		scanNumber += 1
		lastSensorReadings
	}//end scan()
	
	
	
	def act() {
		link(environment)
		println("Agent running")
		
		loop 
		{
			react
			{
              case "Start" => {
            	  worldDimensions 
            	  println("Creating initial state")
            	  val initial = stateFactory.getCoordinate(0, 0)
            	  currentState = initial
            	  println("Creating initial goal")
            	  val goal = stateFactory.getCoordinate(0, -goalIncrement)
            	  println("Got initial goal")
            	  //scan
            	  //initialScan
            	  explore(initial,goal)
              }//end case "Start"
              
			}//end react
		}//end loop
	}
	
}