package collective2

import scala.actors._
import Actor._
import scala.collection.immutable.TreeMap
import uncertaintyMath.Measurement
import uncertaintyMath.Measurement._
import collective2.definitions._
import focusedDstar._
import timestampConcurrency._

class Agent(val environment: Actor, val collectiveMap: Actor,
            val sensorRange: Double, val sensorDeltaAngle: Double, val sensorDeltaRange: Double,
            override val obstacleCost: Double = 1E6, val maxTransactionAttempts: Int = 20,
            testMode: Boolean = false) 
            extends  Actor with CartesianCoordinateOneUnitDiagonalDStar
{
	private[this] val waitTime: Long = 100//ms
	
	setStateTransitionOperation(moveTo)//must execute before moveAgent called
	private[this] var exploredArea = new QuadBitSet
	private[this] var obstacles = new QuadTree[Int]//Stores obstacle type at obstacle location
	private[this] var collectiveObstacles = new QuadTree[Int]//Stores the id at obstacle location
	
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
	
	def changeGoal(x: Int,y: Int): CoordinateState = {
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
				println("Finished Exploring Around ("+x+","+y+")")
		goal
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
		//expandCollectiveMap
		
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
	private[this]var newObstacles =  collection.mutable.Set.empty[(Int,Int,Int)]
	def createCollectiveMapSensorReadings(): List[ScannedObstacle] = {
		scan()
		var result: List[(Int,Displacement,Int)] = Nil
		val obstacleList = obstacles.toList
		println("ObstacleList: "+obstacleList)
		for(newObstacle <- newObstacles.toList)yield
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
								relations = (new Displacement(obstacleX-newObstacleX,
									obstacleY-newObstacleY),obstacleType)::relations
						}//end if self	
					}//end for
					relations
				}
			)//end ScannedObstacle	
		}//end for newObstacles
	}
	
	def testPotentialCollectiveMapMatches(transaction: Transaction,potentialMatches: List[PotentialMatch]): (Boolean,Option[List[PotentialMatch]]) = {
		var successful = true
		val remainingMatches = potentialMatches.filter( potentialMatch => 
			{
				val PotentialMatch(x, y, mapObstacle) = potentialMatch
				println(potentialMatch)
				//if(x == -4 && y == -8)
				//	println()
				var possible = true
				
				collectiveObstacles(x,y) match{
					case Some(collectiveObstacleID)=>{
						val mapObstacleFuture = collectiveMap !! GetCollectiveObstacle(collectiveObstacleID)
						mapObstacleFuture() match {
							case Some(returnedMapObstacle @ CollectiveObstacle(_,_,_)) => {
								returnedMapObstacle == mapObstacle
							}
							case other => throwException("Expected collectiveObstacle from id "+collectiveObstacleID
									+" but received: "+other)
						}
					}
					case None=>{
						var (readSuccessful,relationVectors) = mapObstacle.getRelationVectors(transaction)
						if(readSuccessful){
							while(possible && !relationVectors.isEmpty){
								var (vector,obstacleType) = relationVectors.head
								relationVectors = relationVectors.tail
								val relationX = x + vector.x.value.toInt
								val relationY = y + vector.y.value.toInt
								resetExplore()
								var isExplored = explored(relationX,relationY) 
								var goal = stateFactory.getCoordinate(relationX, relationY)
								if(!isExplored && goal != null){
									val resultPath = moveAgent(currentState,goal)//will move agent within sensor range if it is possible
									if(resultPath.isUnreachable){
										exploredArea.add(relationX,relationY)
										possible = false
										successful = false
									}
								}else{
									//if relation not seen, not match
									if(!obstacles.containsElementAt(x,y))
										possible = false
										/*
							val relation: Displacement = vector + new Displacement(x,y)
							if( obstacles.containsElementAt(x,y) &&
									!mapObstacle.possibleMatchTest( List( (relation, obstacles(x,y).asInstanceOf[Int]) ) ) ){
								possible = false
							}
										 */
								}//end else !explored

							}//end while possible
							possible
						}else{
							successful = false
							false
						}//end else readSuccessful
					}//end case None
				}//end match collectiveObstacle(x,y)	
			}
		)//end filter potentialMatches
		if(successful){
			import collection.mutable.DoubleLinkedList
			var resultMatches =  DoubleLinkedList( remainingMatches: _*)
			var obstacleSet = collection.mutable.Set.empty[(Int,Int)]
			var multipleMatchesSet = collection.mutable.Set.empty[(Int,Int)]
			for(remainingMatch<-remainingMatches){
				val PotentialMatch(matchX, matchY, _) = remainingMatch
				if(!obstacleSet.contains(matchX,matchY))
					obstacleSet += ((matchX,matchY))
				else{
					multipleMatchesSet += ((matchX,matchY))
					resultMatches = resultMatches.filterNot(potentialMatch => 
						{
							val PotentialMatch(x, y, _) = potentialMatch
							x == matchX && y == matchY
						}
					)
					println("Several possiblilities remain for ("+matchX+","+matchY+")")
					//(false,None)
				}
			}
			
			if(!resultMatches.isEmpty)
				(true,Some(resultMatches.toList))
			else
				(true,None)
		}else{//else not successful
			(false,None)
		}
	}
	//private[this] var collectiveObstacles = new QuadTree[CollectiveObstacle]
	
	def createNewCollectiveObstacle(transaction: Transaction, scannedObstacle: ScannedObstacle): Boolean = {
		val ScannedObstacle(scannedX,scannedY,obstacleType,scannedRelations) = scannedObstacle	
			
		var obstacleName  = randomGenerator.nextInt
		var collectiveObstacle = {
			(collectiveMap !! GetCollectiveObstacle(obstacleName))
		}
		while(collectiveObstacle() != None){
			obstacleName  = randomGenerator.nextInt
			collectiveObstacle = {
				(collectiveMap !! GetCollectiveObstacle(obstacleName))
			}
		}
		val sensorArea: List[Coordinate] = {
			exploredArea.XYs.map(xy => {
					val (x,y) = xy
					Coordinate(x-scannedX,y-scannedY)
				}
			)
		}
		println("Explored area of new collective obstacle: "+sensorArea)
		val relations: Map[Displacement,(Int,Option[CollectiveObstacle])] = {
							val relationsList = scannedRelations.map(
										relation => {
											val (vector,obstacleType) = relation
											(vector,(obstacleType,None))
									}
							)
							collection.immutable.Map(relationsList: _*)
		}//end relations
		println("Relations of new collective obstacle: "+relations)
		val newCollectiveObstacle = new CollectiveObstacle(obstacleType,relations,sensorArea)
		var resultFuture = (collectiveMap !! AddCollectionObstacle(transaction,obstacleName,
			newCollectiveObstacle)//end AddCollectionObstacle
		).asInstanceOf[Future[Boolean]]
		if( resultFuture() ){
			collectiveObstacles.add(scannedX, scannedY, obstacleName)
			return true
		}else{
			println("Failed to add following new obstacle to CollectiveMap: "+newCollectiveObstacle)
			return false
		}
	}
	
	/*
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
	*/
	
	def submitDataToCollectiveMap(transaction: Transaction, remainingMatch: Option[List[PotentialMatch]],
			scannedObstacles: List[ScannedObstacle]): Boolean = {
		
		for(scannedObstacle <- scannedObstacles){
			//scannedObstacle is really an unsubmitted object
			//ScannedObstacle(x: Int,y: Int,obstacleType: Int,scannedRelations: List[(Displacement,Int)])
			/*CollectiveObstacle(val obstacleType: Int,
				private var relations: TreeMap[Displacement,CollectiveObstacle],
				sensorArea: List[Coordinate])
			*/
			val ScannedObstacle(scannedX,scannedY,scannedType,scannedRelations) = scannedObstacle
			remainingMatch match {
				case Some(matches @ List(PotentialMatch(_,_,_) ,_*)) =>{
					for(potentialMatch<-matches)
					{
						val PotentialMatch(x, y, mapObstacle)= potentialMatch
						//create new CollectiveObstacles, except mapObstacle
						//match them with the appropriate Displacements in Seq[(Displacement,CollectiveObstacle]
						//val scannedVectors: List[Displacement] = scannedRelations.map( scannedRelation => scannedRelation._1 )
						if(x == scannedX && y == scannedY){
							val (readSuccessful,mapObstacleRelations)=mapObstacle.getRelationVectors(transaction)
							if(readSuccessful){
								val newObstacleRelations = mapObstacleRelations.diff(scannedRelations)
								var updateSuccessfulFuture = collectiveMap !! UpdateCollectiveObstacle(transaction,mapObstacle,
										newObstacleRelations.map(
												relation => {
													val (vector,obstacleType) = relation
													(vector,(obstacleType,None))
												}
										)//end map
								)//end UpdateCollectiveOpbstacle
								if(updateSuccessfulFuture().asInstanceOf[Boolean]){
									println("CollectiveObstacle Update was Successful")
								}
							}//end if readSuccessful
							return false
						}//end if scannedObstacle is the same as the PotentialMatch
						else{
							if(!createNewCollectiveObstacle(transaction,scannedObstacle))
								return false
						}//end else
					}
				}//end case Some(PotentialMatch)
				case None => {
					//create new CollectiveObstacles, except mapObstacle
				//match them with the appropriate Displacements in Seq[(Displacement,CollectiveObstacle]
					if(!createNewCollectiveObstacle(transaction,scannedObstacle))
						return false
				}
				case error => throwException("Expected PotentialMatch but received"+error)
			}	
		}
		true
	}
	
	def expandCollectiveMap(){
		val transaction = new Transaction("Scan"+scanNumber+" Transaction",maxTransactionAttempts)
				transaction.setOperations(
					{
						val scannedObstacles = createCollectiveMapSensorReadings
						println("ScannedObstacles: "+scannedObstacles)
						var possibleResultsFuture = (collectiveMap !! 
							GetPossibleStates(transaction,scannedObstacles) )
						possibleResultsFuture() match {
							case OperationResult(true,pMatches) => {
								var potentialMatches = pMatches.asInstanceOf[List[PotentialMatch]]
								println("PotentialMatches: "+potentialMatches)
								val (successful,remainingMatch) =testPotentialCollectiveMapMatches(
										transaction,potentialMatches)
								if(successful){
									println("RemainingMatch: "+remainingMatch)
									submitDataToCollectiveMap(transaction,remainingMatch,scannedObstacles)
								}else
									false//retry transaction
							}
							case OperationResult(false,_) => {false}
							case error => throwException("Transaction "+transaction.name+
														" expected OperationResult but "+
														"received CollectiveMap reply of "+error)
						}//end match possibleResultsFuture()
					}
				)//end setOperations
				newObstacles =  collection.mutable.Set.empty[(Int,Int,Int)]
	}
	
	def scan(): Map[(State,State),Double] = {
		println("Scanning")
		val scanFuture = environment !! UpdateSensor(this, sensorRange, sensorDeltaAngle, sensorDeltaRange)
		val Scan(relativeScannedArea,detectedObstacles,detectedAgents) = scanFuture()
		println("Updating explored area")
		var currentX,currentY=0
		currentState match{
					case CoordinateState(x,y,_,_) => {
						currentX=x
						currentY=y
					}
					case state => {
						throwException(state+" is an incompatable State")
					}
				}//end currentState match
		val scannedArea=relativeScannedArea.XYs.map(element => {
				val (x,y)=element
				(x+currentX,y+currentY)
			}
		)
		exploredArea.add(scannedArea: _*)
		println("ExploredArea: "+exploredArea)
		import scala.collection.mutable
		var tempMap = mutable.Map.empty[(State,State),Double]
		//println("Getting currentState")
		currentState match {
			case state @ CoordinateState(x,y,_,_) => {
				var scannedStates: List[(Int,Int)] = for(xy <- relativeScannedArea.XYs)yield {
					val (relativeX,relativeY) = xy
					val horiz = x + relativeX
					val vert = y + relativeY
					(horiz,vert)
				}
				var scannedStatesIterator = scannedStates.toIterator
				//add costs to all areas scanned, assuming empty
				//println("Adding all costs as passable transitions")
				//println("scannedArea: "+scannedArea.XYs)
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
							newObstacles += ((horiz,vert,obstacleType))
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
            	  if(!testMode){
            		  val goal = stateFactory.getCoordinate(0, -goalIncrement)
            		  println("Got initial goal")
            		  //initialScan()
            		  explore(initial,goal)
            	  }
            	  //scan
            	  //initialScan
            	  
              }//end case "Start"
              case TestGoal(x,y)=>{
            	  val goal = stateFactory.getCoordinate(x, y)
            	  println("Got initial goal")
            	  //initialScan()
            	  //pathfinding doesn't try to move through the obstacle if
            	  //it is detected before any planning occurs
            	  //otherwise tries to move through the obstacle as previously planned
            	  moveAgent(currentState,goal)
              }
              case TestMapProducer => {
            	  moveAgent(currentState,stateFactory.getCoordinate(0, -2))
            	  expandCollectiveMap()
            	  //println(collectiveMap)
            	  moveAgent(currentState,stateFactory.getCoordinate(-1, -3))
            	  moveAgent(currentState,stateFactory.getCoordinate(-1, -4))
            	  expandCollectiveMap()
            	  //println(collectiveMap)
              }
			}//end react
		}//end loop
	}
	
}