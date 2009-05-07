

import scala.actors._
import Actor._
import Measurement._
//import agents.DataClasses
import scala.util.Random

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
}
case class Move(agent: Actor, x: Measurement, y: Measurement) 
case class ObjectReading(angle: Measurement, distance: Measurement, obstacleType:Int)
case class AgentReading(angle: Measurement, distance: Measurement) 
case class TopologicalEntry(obstacle1Type: Int,obstacle2Type: Int,
                            deltaX: Measurement, deltaY: Measurement) 
case class IdentifiedObject(identifier1: Int, identifier2: Int, 
                            obstacle1Type: Int,obstacle2Type: Int,
                            deltaX: Measurement, deltaY: Measurement) 
case class UpdateSensor(sender: Agent, range: Int, sensorDeltaAngle: Int, SensorDeltaRange: Int) 

case class Coordinate(x: Int, y: Int) 
case class Obstacle(obstacleType: Int, x: Int, y: Int){
	override def toString = "Obstacle: obstacleType="+obstacleType+" x="+x+" y="+y
} 

case class Goal(goal:Obstacle)
case class GoalNotFound()

class Agent(val environment: Actor, val map: Actor,
            val sensorRange: Int, val sensorDeltaAngle: Int, val SensorDeltaRange: Int) extends Actor 
{
  private val topologicalElementGenerator: Actor = new TopologicalElementGenerator(map)
  private val relationshipIdentfier: Actor = new RelationshipIdentfier(map)
  private val mapUpdatePoller: Actor = new MapUpdatePoller(this,map)
  private val goalFinder: Actor = new GoalFinder(this,map)
  topologicalElementGenerator.start
  relationshipIdentfier.start
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
  private var visibleAgentsCurrent: Boolean = false
  private var visibleObstaclesCurrent: Boolean = false
  
  override def toString = {
	var result = "Agent: sensorRange=" + sensorRange + " sensorDeltaAngle=" + sensorDeltaAngle + " SensorDeltaRange=" + SensorDeltaRange
	result += " environment=" + environment + " topologicalElementGenerator=" + topologicalElementGenerator
	result +=  " relationshipIdentfier=" + relationshipIdentfier + " map=" + map
	result
	}
  
  def move(x: Int,y: Int){
		environment ! MoveCommand(this,x,y)
	}

  def idealMove(x: Int,y: Int) {
      if(visibleObstacles.contains(Displacement(x,y)))
      {
         var x2 = 0
         if(visibleObstacles.contains(Displacement(x2,y)))
         {
                var y2 = 0
                if(visibleObstacles.contains(Displacement(x,y2)))
                {
                   x2 = -x
                   if(visibleObstacles.contains(Displacement(x2,y)))
                   {
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
    if(num > 0)
        return 1
    else if (num < 0)
        return -1
    else
        return 0
  }

  def addToMapMethod(entries: IdentifiedObject *)
  {
    entries.foreach(entry => map ! Add(entry))
  }

  def planMovement() {
      println("Planning Movement")
      if(!visibleAgentsCurrent)
      {
        for(displacement <- visibleAgents){
            visibleAgents -= displacement
            visibleAgents += Displacement(displacement.x+lastDisplacementX,displacement.y+lastDisplacementY)
        }
      }
      if(!visibleObstaclesCurrent)
      {
          for(displacement <- visibleObstacles.keySet){
              visibleObstacles.get(displacement) match {
                  case Some(obstacleType) => {
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
      idealMove(flattenToOne(toTargetX.value),flattenToOne(toTargetY.value))
      if(visibleObstacles.contains(Displacement(toTargetX,toTargetY)))//see a marker
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
				environment ! UpdateSensor(this, sensorRange, sensorDeltaAngle, SensorDeltaRange)
                if(exploreMode)
                    move(randomPositiveNegative1(),randomPositiveNegative1())
                else if(!pathToGoal.isEmpty)
                    planMovement
			  }
			  case sensorReadings @ List(ObjectReading,_*) => {
                topologicalElementGenerator ! sensorReadings
			    //pass to helper actor that calculates topological references and sends results as a message to parent actors
                visibleObstacles.clear//empties the map
                //add detect obstacles to the map
                for(objectReading <- sensorReadings.asInstanceOf[List[ObjectReading]])
                {
                    val displacement = PolarToCartesian(objectReading.angle,objectReading.distance)
                    if(displacement != Displacement(0,0)){
                        visibleObstacles += Pair(displacement, objectReading.obstacleType)
                        //println("Angle: " + objectReading.angle + " Distance: " + objectReading.distance)
                        //println("Displacement generated: " + displacement)
                    }
                }
                visibleObstaclesCurrent = true
                if(!visibleObstacles.isEmpty)
                    println(visibleObstacles)
              }
              case sensorReadings @ List(AgentReading(angle,distance),_*) => {
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
                /*
                if(!visibleAgents.isEmpty)
                    println(visibleAgents)
                */
              }
			  case topologicalEntries @ List(TopologicalEntry, _*) =>
			    relationshipIdentfier ! topologicalEntries
			    //send to helper actor that identifies the objects, naming if necessary, messages to parent identify objects
			  case relationships @  List(IdentifiedObject, _*) => {
				  addToMapMethod(relationships.asInstanceOf[Seq[IdentifiedObject]]: _*)//asInstanceOf is a cast, need to test that works correctly
				  //move(randomPositiveNegative1(),randomPositiveNegative1())
                  if(exploreMode)
                    move(randomPositiveNegative1(),randomPositiveNegative1())
			  }
              case Goal(goal) => {
                      this.goal = goal
                      goalSet = true
              }
              case path @ List(Coordinate, _*) => {
                      pathToGoal = path.asInstanceOf[List[Coordinate]]
                      locationDuringPlanX = relativeLocationX
                      locationDuringPlanY = relativeLocationY
              }
              case "Stop Exploring" => {
                    exploreMode = false
                    println("Leaving Explore Mode")
                    if(goalSet){
                        goalFinder.start
                        goalFinder ! Goal(goal)
                    }
                    else
                        println("You never sent me a goal")
              }
              case TargetDisplacement(xDisplacementToGoal,yDisplacementToGoal) => {
                      //drive to goal, avoiding obstacles
              }
              case GoalNotFound() => {
                      exploreMode = true
                      println("Entering Explore Mode")
                      goalFinder ! "Exit"
              }
              case "Exit" => {
                 println("Agent Exiting")
                 topologicalElementGenerator ! "Exit"
                 relationshipIdentfier ! "Exit"
                 goalFinder ! "Exit"
                 mapUpdatePoller ! "Exit"
                 this.exit
              }
			}//end react
		}//end loop
	}//end act
 }//end agent class



//import agents.QuadTree
//import agents.ObstacleJava

