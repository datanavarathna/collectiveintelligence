

import scala.actors._
import Actor._
import Measurement._
//import agents.DataClasses
import scala.util.Random

case class Pheromone(locationX: Int,LocationY: Int,targetX: Int, targetY: Int)
case class MoveCommand(sender: Agent, x: Int, y: Int ) 
case class Displacement(x: Measurement, y: Measurement) 
case class Move(agent: Actor, x: Measurement, y: Measurement) 
case class ObjectReading(angle: Measurement, distance: Measurement) 
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

  private var exploreMode: Boolean = true
  private var goal: Obstacle = _
  private var goalSet: Boolean = false
  
  override def toString = {
	var result = "Agent: sensorRange=" + sensorRange + " sensorDeltaAngle=" + sensorDeltaAngle + " SensorDeltaRange=" + SensorDeltaRange
	result += " environment=" + environment + " topologicalElementGenerator=" + topologicalElementGenerator
	result +=  " relationshipIdentfier=" + relationshipIdentfier + " map=" + map
	result
	}
  
  def move(x: Int,y: Int){
		environment ! MoveCommand(this,x,y)
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
  
  def addToMapMethod(entries: IdentifiedObject *)
  {
    entries.foreach(entry => map ! Add(entry))
  }
  
	def act()
	{
		println("Agent running")
        if(exploreMode)
            move(randomPositiveNegative1(),randomPositiveNegative1())
		loop 
		{
			react
			{
			  case Displacement( x, y) => 
			  {
                lastDisplacementX = x.value.toInt
                lastDisplacementY = y.value.toInt
                relativeLocationX += x
				relativeLocationY += y 
				environment ! UpdateSensor(this, sensorRange, sensorDeltaAngle, SensorDeltaRange)
                if(exploreMode)
                  move(randomPositiveNegative1(),randomPositiveNegative1())
			  }
			  case sensorReadings @ List(ObjectReading(angle,distance),_*) =>
			    topologicalElementGenerator ! sensorReadings
			    //pass to helper actor that calculates topological references and sends results as a message to parent actors
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
              case "Stop Exploring" => {
                    exploreMode = false
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
              case GoalNotFound() => exploreMode = true
              case "Exit" => {
                 println("Agent Exiting")
                 topologicalElementGenerator ! "Exit"
                 relationshipIdentfier ! "Exit"
                 this.exit
              }
			}//end react
		}//end loop
	}//end act
 }//end agent class



//import agents.QuadTree
//import agents.ObstacleJava

