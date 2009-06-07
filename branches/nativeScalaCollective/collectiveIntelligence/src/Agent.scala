

import scala.actors._
import Actor._
import Measurement._
//import agents.DataClasses
import scala.util.Random
//import TopologicalElementGenerator._
//import CaseClasses._
/*
case class Pheromone(locationX: Int,LocationY: Int,targetX: Int, targetY: Int)
case class MoveCommand(sender: Agent, x: Int, y: Int ) 
case class Displacement(x: Measurement, y: Measurement) {
    def canEqual(other: Any): Boolean = { other.isInstanceOf[Displacement] }
	override def equals(other:Any):Boolean =
	{
		other match {
		  case other: Displacement =>
		    {
		      //the "2 *" is for 95% certainty
		      (other canEqual this) &&
              this.x == other.x && this.y == other.y
		    }
          case _ => false
		}
	}

    def - (that: Displacement): Displacement = {
        new Displacement(this.x - that.x,this.y-that.y)
    }

    def inverse(): Displacement = {
        Displacement(-1*this.x, -1*this.y)
    }
}
case class Move(agent: Actor, x: Measurement, y: Measurement) 
case class ObjectReading(angle: Measurement, distance: Measurement, obstacleType:Int)
case class AgentReading(angle: Measurement, distance: Measurement) 
case class TopologicalEntry(obstacle1Type: Int,obstacle2Type: Int,
                            deltaX: Measurement, deltaY: Measurement){
    def inverse() : TopologicalEntry = {
        new TopologicalEntry(obstacle2Type,obstacle1Type,-1*deltaX,-1*deltaY)
    }
}
case class IdentifiedObject(identifier1: Int, identifier2: Int, 
                            //obstacle1Type: Int,obstacle2Type: Int,
                            vector: Displacement){
    def inverse: IdentifiedObject = {
        IdentifiedObject(identifier2,identifier1,vector.inverse)
    }
}
case class UpdateSensor(sender: Agent, range: Int, sensorDeltaAngle: Int, SensorDeltaRange: Int) 

case class Coordinate(x: Int, y: Int) 
case class Obstacle(obstacleType: Int, x: Int, y: Int){
	override def toString = "Obstacle: obstacleType="+obstacleType+" x="+x+" y="+y
} 

case class Goal(goal:Obstacle)
case class GoalNotFound()
*/
class Agent(val environment: Actor, val map: Actor,
            val sensorRange: Int, val sensorDeltaAngle: Int, val SensorDeltaRange: Int) extends Actor 
{
  private val sensorProcessor = new SensorObjectReadingsProcessor(map, this)
  private val mapUpdatePoller: Actor = new MapUpdatePoller(this,map)
  private val goalFinder: Actor = new GoalFinder(this,map)
  sensorProcessor.start
  mapUpdatePoller.start

  private var relativeLocationX: Measurement = new Measurement(0.00000001,0)
  private var relativeLocationY: Measurement = new Measurement(0.00000001,0)
  private val randomGenerator: Random = new Random
  private var lastDisplacementX: Int = 0
  private var lastDisplacementY: Int = 0
  private var locationDuringPlanX: Measurement = relativeLocationX
  private var locationDuringPlanY: Measurement = relativeLocationY

  private var exploreMode: Boolean = true
  private var goal: Obstacle = _
  private var goalSet: Boolean = false

  private var detectedAgents: List[AgentReading] = Nil
  private var pathToGoal: List[Coordinate] = Nil
  private var xFromLastTarget: Int = 0
  private var yFromLastTarget: Int = 0

  import scala.collection.mutable.Set
  import scala.collection.mutable.Map
  private var visibleAgents = Set.empty[Displacement]
  private var visibleObstacles = Map.empty[Displacement,Int]//key,value obstacleType: Int
  private var locationOfObstacles = Map.empty[Displacement,Int]//key,value identifier: Int
  private var identifierTranslator = Map.empty[Int,Int]// originalIdentifier->currentIdentifier
  private var visibleAgentsCurrent: Boolean = false
  private var visibleObstaclesCurrent: Boolean = false

  private var stoppedExploring: Boolean = false

  override def toString = {
	var result = "Agent: sensorRange=" + sensorRange + " sensorDeltaAngle=" + sensorDeltaAngle + " SensorDeltaRange=" + SensorDeltaRange
	result += " environment=" + environment + " SensorObjectReadingsProcessor=" + sensorProcessor
	result +=  " map=" + map
	result
	}

  def visibleAgentsContains(xy: Displacement): Boolean = {
      println("visibleAgentsContains: " + visibleAgents)
      for(visibleAgentCoord <- visibleAgents)
      {
          if(xy == visibleAgentCoord)
            return true
          else
            println(xy + "!=" + visibleAgentCoord)
      }
      return false
  }
  def visibleObstaclesContains(xy: Displacement): Boolean = {
      println("visibleObstaclesContains: " + visibleObstacles)
      for(visibleObstacleCoord <- visibleObstacles.keySet)
      {
          if(xy == visibleObstacleCoord)
            return true
          else
            println(xy + "!=" + visibleObstacleCoord)
      }
      return false
  }

  def move(x: Int,y: Int){
      //Thread.sleep(100)
		environment ! MoveCommand(this,x,y)
	}

  def idealMove(x: Int,y: Int) {
      println("1st: ("+x+","+y+")")
      if(visibleObstacles.contains(Displacement(x,y)))
      {
         var x2 = 0
         println("2nd: ("+x2+","+y+")")
         if(visibleObstacles.contains(Displacement(x2,y)))
         {
                var y2 = 0
                println("3rd: ("+x+","+y2+")")
                if(visibleObstacles.contains(Displacement(x,y2)))
                {
                   x2 = -x
                   println("4th: ("+x2+","+y+")")
                   if(visibleObstacles.contains(Displacement(x2,y)))
                   {
                      println("5th: ("+x2+","+y+")")
                      y2 = -y
                      move(x,y2)
                   }//if obstacle 4th attempt
                   else
                      move (x2,y)
                }//if obstacle in 3rd attempt
                else
                    move(x,y2)
         }//if obstacle in 2nd attempt
         else
            move(x2,y)
      }//if obstacle at 1st attempt
      else
        move(x,y)
  }

  def PolarToCartesian(angle: Measurement, distance: Measurement): Displacement =  {
      Displacement(distance * cos(angle), distance * sin(angle))
  }

  def randomPositiveNegative1(): Int = {
	  /*
      if (randomGenerator.nextBoolean)
	    return -1
	  else
	    return 1
       */
      return randomGenerator.nextInt(3) - 1
  }

  def flattenToOne(num: Double): Int =
  {
    if(Math.abs(num) < 0.001)
        return 0
    else if(num > 0)
        return 1
    else
        return -1
  }

  def addToMapMethod(lastUpdate: Long, entries: List[IdentifiedObject])
  {
    //entries.foreach(entry => map ! Add(lastUpdate,entry))
    map ! Add(lastUpdate,entries)
  }

  def planMovement() {
      println("Planning Movement")
      if(!visibleAgentsCurrent)//doesn't appear to have an effect
      {
        println("updating visibleAgents: " + visibleAgents)
        for(displacement <- visibleAgents){
            println("old Visible Displacement: " + displacement)
            visibleAgents -= displacement
            visibleAgents += Displacement(displacement.x+lastDisplacementX,displacement.y+lastDisplacementY)
        }
      }
      if(!visibleObstaclesCurrent)
      {
          println("updating visibleObstacles: " + visibleObstacles)
          for(displacement <- visibleObstacles.keySet){
              visibleObstacles.get(displacement) match {
                  case Some(obstacleType) => {
                          println("old visibleObstacle: " + displacement)
                          visibleObstacles -= displacement
                            visibleObstacles += Pair(
                                Displacement(displacement.x+lastDisplacementX,
                                             displacement.y+lastDisplacementY),obstacleType)
                  }
                  case None => println("Map does not contain a value for key: " + displacement)
              }

          }
      }
      val target:Coordinate = pathToGoal.head
      val currentX = relativeLocationX - locationDuringPlanX
      val currentY = relativeLocationY - locationDuringPlanY
      val toTargetX = (target.x + xFromLastTarget) - currentX
      val toTargetY = (target.y + yFromLastTarget) - currentY
      println("target: " + target)
      println("current location: ("+currentX+","+currentY+")")
      println("toTarget: ("+toTargetX+","+toTargetY+")")
      idealMove(flattenToOne(toTargetX.value),flattenToOne(toTargetY.value))
      if(visibleObstaclesContains(Displacement(toTargetX,toTargetY)))//see a marker
      {
          pathToGoal = pathToGoal.tail
          if(pathToGoal.isEmpty)//see goal
          {
              //move as close to goal as possible

              println("Found goal")
              this.exit
          }

      }//end if see marker
      //else
      //move toward target without running into anything
  }

	def act()
	{
		println("Agent running")
        
		loop 
		{
			react
			{
              case "Start" => {
                  //Thread.sleep(100)
                  if(exploreMode)
                    move(randomPositiveNegative1(),randomPositiveNegative1())
                  else if(!pathToGoal.isEmpty)
                    planMovement
              }
			  case Displacement( x, y) => 
			  {
                /*
                if(x != new Measurement(0,.005) && y != new Measurement(0,.005))
                    println("Agent moved: (" + x + "," + y + ")")
                */
                lastDisplacementX = x.value.toInt
                lastDisplacementY = y.value.toInt
                relativeLocationX += x
				relativeLocationY += y 
                visibleAgentsCurrent = false
                visibleObstaclesCurrent = false
                //println("Displaced: (" +lastDisplacementX+","+lastDisplacementY+")" )
                
                //update sensor before moving again
				environment ! UpdateSensor(this, sensorRange, sensorDeltaAngle, SensorDeltaRange)
                if(exploreMode)
                    move(randomPositiveNegative1(),randomPositiveNegative1())
                else if(!pathToGoal.isEmpty)
                    planMovement
			  }
     
			  //from update sensor
			  case sensorReadings @ List(ObjectReading(_,_,_),_*) => {
                //println("Received visible objects: " + sensorReadings)
                
                //pass to helper actor that calculates topological references and sends results as a message to parent actors
                sensorProcessor ! sensorReadings
                
                visibleObstacles.clear//empties the map
                //add detect obstacles to the map
                for(objectReading <- sensorReadings.asInstanceOf[List[ObjectReading]])
                {
                    //println("objectReading: " + objectReading)
                    val displacement = PolarToCartesian(objectReading.angle,objectReading.distance)
                    //if(displacement != Displacement(0,0))
                    if(displacement.x.value != 0 && displacement.x.uncertainty != 0 &&
                       displacement.y.value != 0 && displacement.y.uncertainty != 0 )
                    {
                        //println("Added obstacle to visible list")
                        visibleObstacles += Pair(displacement, objectReading.obstacleType)
                        //println("Angle: " + objectReading.angle + " Distance: " + objectReading.distance)
                        //println("Displacement generated: " + displacement)
                        
	                    
                    }
                    //println("Angle: " + objectReading.angle + " Distance: " + objectReading.distance)
                    //println("Displacement generated: " + displacement)
                }
                visibleObstaclesCurrent = true
                //if(!visibleObstacles.isEmpty)
                    //println("visibleObstacles: " + visibleObstacles)
              }
              case sensorReadings @ List(AgentReading(angle,distance),_*) => {
                println("Received visible agents")
                detectedAgents = sensorReadings.asInstanceOf[List[AgentReading]]
                visibleAgents.clear//empties the set
                //add detect agents to the set
                for(agentReading <- sensorReadings.asInstanceOf[List[AgentReading]])
                {
                    val displacement = PolarToCartesian(agentReading.angle,agentReading.distance)
                    if(displacement != Displacement(0,0)){
                        visibleAgents += displacement
                        //println("Angle: " + angle + " Distance: " + distance)
                        //println("Displacement generated: " + displacement)
                    }

                }
                visibleAgentsCurrent = true
                
                if(!visibleAgents.isEmpty)
                    println(visibleAgents)
                
              }
              /*
              //received from topological element generator
			  case topologicalEntries @ List(TopologicalEntry(_,_,_,_), _*) => {
			    //send to helper actor that identifies the objects, naming if necessary, messages to parent identify objects
			    relationshipIdentifier ! topologicalEntries
			    println("Received topological entries")
			  }
			  */
              case recheck @ RecheckObjects(identifiedObjects) => {
            	  sensorProcessor.forward(recheck)
              }
     
			  case NewIdentifiedObjects(lastUpdate,newIdentifiedObjects) => {
				  println("Adding  the following relationships to CollectiveMap: " + newIdentifiedObjects)
				  addToMapMethod(lastUpdate, newIdentifiedObjects)//asInstanceOf is a cast, need to test that works correctly
				  map ! MapSize
                  if(exploreMode)
                    move(randomPositiveNegative1(),randomPositiveNegative1())
			  }
              case Goal(goal) => {
                      println("Goal Set")
                      this.goal = goal
                      goalSet = true
              }
              case path @ List(Coordinate(_,_), _*) => {
                      println("Received path: " + path)
                      pathToGoal = path.asInstanceOf[List[Coordinate]]
                      locationDuringPlanX = relativeLocationX
                      locationDuringPlanY = relativeLocationY
                      planMovement
              }
              case "Stop Exploring" => {
                    if(!stoppedExploring)
                    {
                        exploreMode = false
                        println("Leaving Explore Mode")
                        if(goalSet){
                            goalFinder.start
                            goalFinder ! Goal(goal)
                        }
                        else
                            println("You never sent me a goal")
                        stoppedExploring = true
                    }

                    
              }
//              case TargetDisplacement(xDisplacementToGoal,yDisplacementToGoal) => {
//                      //drive to goal, avoiding obstacles
//              }
              case GoalNotFound() => {
                      stoppedExploring = false
                      exploreMode = true
                      println("Entering Explore Mode")
                      goalFinder ! "Exit"
              }
              case "Exit" => {
                 println("Agent Exiting")
                 goalFinder ! "Exit"
                 mapUpdatePoller ! "Exit"
                 this.exit
              }
              //case catchAll => println("Agent Catchall: " + catchAll)
			}//end react
		}//end loop
	}//end act
 }//end agent class



//import agents.QuadTree
//import agents.ObstacleJava

