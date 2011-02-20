package collective2

import scala.actors._
import Actor._
import uncertaintyMath.Measurement
import uncertaintyMath.Measurement._
import collective2.definitions._
import scala.util.Random
import focusedDstar._

case class detectedObstacles()

class Agent(val environment: Actor, val collectiveMap: Actor,
            val sensorRange: Double, val sensorDeltaAngle: Double, val sensorDeltaRange: Double,
            override val obstacleCost: Double = 1E6) 
            extends  Actor with CartesianCoordinateOneUnitDiagonalDStar
{
	private[this] val waitTime: Long = 100//ms
	
	setStateTransitionOperation(moveTo)//must execute before moveAgent called
	private[this] var exploredArea = new QuadBitSet
	
	private[this] var exploreMode = true
	
	private[this] val mainActor = self
	/*
	private var relativeLocationX: Measurement = new Measurement(0.00000001,0)
	private var relativeLocationY: Measurement = new Measurement(0.00000001,0)
	*/
	
	//private[this] var worldWidth: Int = _
	//private[this] var worldHeight: Int = _
	
	
	
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
	
	def explore(start: State, goal: State){
		println("Exploring to "+goal)
		scan
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
		scan
		Thread.sleep(waitTime)
		println("Sending move command")
		val Displacement( deltaX, deltaY) = (environment !! MoveCommand(this,x,y))()
		println("Moved ("+deltaX+","+deltaY+")")
		(deltaX != 0) || (deltaY != 0)//moved if true
	}
	
	def sensor: Map[(State,State),Double] = scan
	
	def processDetectedObstacles(detectedObstacles: List[ObjectReading]): Iterable[(Int,Int)] = {
		return for(obstacle <- detectedObstacles) yield{
			 PolarToCartesian(obstacle.angle,obstacle.distance).toIntInt	
		}
	}
	
	def processDetectedAgents(detectedAgents: List[AgentReading]): Iterable[(Int,Int)] = {
		return for(agent <- detectedAgents) yield{
			 PolarToCartesian(agent.angle,agent.distance).toIntInt	
		}
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
							val (relativeX,relativeY) = xy
							val horiz = x + relativeX
							val vert = y + relativeY
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
            	  scan
            	  initialScan
            	  explore(initial,goal)
              }//end case "Start"
              
			}//end react
		}//end loop
	}
	
}