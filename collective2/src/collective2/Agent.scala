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
            val sensorRange: Double, val sensorDeltaAngle: Double, val sensorDeltaRange: Double) 
            extends  Actor with CartesianCoordinateOneUnitDiagonalDStar
{
	private[this] var exploredArea = new QuadBitSet
	
	private[this] var exploreMode = true
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
    	  worldDimensionsFuture.inputChannel.react{
    	  	case (x: Int,y: Int) => {
    	  		//worldWidth = x
    	  		//worldHeight = y
    	  		stateFactory = new CoordinateCreator((-1*x,-1*y),(x,y))
    	  	}
    	  	case other => throw new Exception(other+" returned when (x,y) expected")
    	  }
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
	
	def explore(goal: State){
		moveAgent(currentState,goal) match {
			/*
			case path: NoPath => {
				
			}
			*/
			case path: Goal => {
				//(path.size <= 1)
				currentState match{
					case CoordinateState(x,y,_,_) => {
						adjustGoal
						if(exploreMode)
							explore(stateFactory.getCoordinate(x+goalXAdjustment, y+goalYAdjustment) )
					}
					case state => {
						throw new Exception(state+" is an incompatable State")
					}
				}//end currentState match
			}
			case other => throw new Exception(other+" returned by moveAgent instead of a Goal")
		}
	}
	/*
	def sensorXYCoordinates = {
		var relativeX = relativeLocationX.value
		var xRange = (relativeX - sensorRange).toInt to (relativeX + sensorRange).toInt
		var relativeY = relativeLocationY.value
		var yRange = (relativeY - sensorRange).toInt to (relativeY + sensorRange).toInt
		(xRange,yRange)
	}
	*/
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
	
	setStateTransitionOperation(moveTo)//must execute before moveAgent called
	
	def moveTo(next: State): Boolean = {
		next match{
			case state: CoordinateState => {
				currentState match{
					case CoordinateState(x,y,_,_) => {
						if(move(state.x -x,state.y-y) )
							return true//moved
						else{//hit an object/edge of map
							updateCostOfTransversal(state, next, Double.PositiveInfinity)
							return false
						}
					}
					case state => {
						throw new Exception(state+" is an incompatable State")
					}
				}//end currentState match
			}//end case state: CoordinateState
			case state => throw new Exception(state+"passed into moveTo is not a CoordinateState")
		}
	}
	
	def move(x: Int,y: Int): Boolean = {
		scan
		(environment !! MoveCommand(this,x,y)).inputChannel.react(
			{
				case Displacement( deltaX, deltaY) => {
					(deltaX != 0) && (deltaY != 0)//moved if true
				}
				case other => throw new Exception("MoveCommand reply was " +other+" instead of a Displacement")
			}
		)
	}
	
	def sensor: Map[(State,State),Double] = lastSensorReadings
	
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
	
	def scan(){
		val scanFuture = environment !! UpdateSensor(this, sensorRange, sensorDeltaAngle, sensorDeltaRange)
		scanFuture.inputChannel.react{
			case Scan(scannedArea,detectedObstacles,detectedAgents) =>{
				exploredArea += scannedArea
				
				import scala.collection.mutable
				var tempMap = mutable.Map.empty[(State,State),Double]
				
				currentState match {
					case state @ CoordinateState(x,y,_,_) => {
						//add costs to all areas scanned, assuming empty
						scannedArea.XYs().foreach(
								xy => {
									val (relativeX,relativeY) = xy
									val horiz = x + relativeX
									val vert = y + relativeY
									val cost = math.max(math.abs(x-relativeX), math.abs(y-relativeY))
									tempMap.put((state,stateFactory.getCoordinate(horiz,vert)), cost)
								}
						)//end scannedArea
						//updating cost for areas with obstalces detected
						processDetectedObstacles(detectedObstacles).foreach( 
								xy => {
									val (relativeX,relativeY) = xy
									val horiz = x + relativeX
									val vert = y + relativeY
									tempMap.put((state,stateFactory.getCoordinate(horiz,vert)), Double.PositiveInfinity)
								}
						)//end processDetectedObstacles
						processDetectedAgents(detectedAgents).foreach( 
								xy => {
									val (relativeX,relativeY) = xy
									val horiz = x + relativeX
									val vert = y + relativeY
									tempMap.put((state,stateFactory.getCoordinate(horiz,vert)), Double.PositiveInfinity)
								}
						)//end processDetectedAgents
						lastSensorReadings = tempMap.toMap[(State,State),Double]
					}//end case state

					case state => {
						throw new Exception(state+" is an incompatable State")
					}
				}//end currentState match
				
			}//end case Scan
			
			case improperValue => throw new Exception("Sensor scan returned the imnproper value: "+improperValue)
		}
	}
	
	
	
	def act() {
		link(environment)
		println("Agent running")
		
		loop 
		{
			react
			{
              case "Start" => {
            	  worldDimensions
            	  explore(stateFactory.getCoordinate(0, goalIncrement))
                  
              }//end case "Start"
			}//end react
		}//end loop
	}
	
}