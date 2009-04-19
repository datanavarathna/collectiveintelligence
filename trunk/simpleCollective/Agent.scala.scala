

import scala.actors._
import Actor._
import Measurement._
//import agents.DataClasses
import scala.util.Random

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

class Agent(val environment: Actor, val topologicalElementGenerator: Actor, val relationshipIdentfier: Actor, val map: Actor,
            val sensorRange: Int, val sensorDeltaAngle: Int, val SensorDeltaRange: Int) extends Actor 
{
    
  var relativeLocationX: Measurement = new Measurement(0.00000001,0)
  var relativeLocationY: Measurement = new Measurement(0.00000001,0)
  val randomGenerator: Random = new Random
  
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
	  if (randomGenerator.nextBoolean)
	    return -1
	  else
	    return 1
  }
  
  def addToMapMethod(entries: IdentifiedObject*)
  {
    map ! entries
  }
  
	def act()
	{
		println("Agent running")
		move(randomPositiveNegative1(),randomPositiveNegative1())
		loop 
		{
			react
			{
			  case Displacement( x, y) => 
			  {
				relativeLocationX += x
				relativeLocationY += y 
				environment ! UpdateSensor(this, sensorRange, sensorDeltaAngle, SensorDeltaRange)
			  }
			  case sensorReadings @ Seq(ObjectReading(angle,distance),_*) =>
			    topologicalElementGenerator ! sensorReadings
			    //pass to helper actor that calculates topological references and sends results as a message to parent actors
			  case topologicalEntries @ Seq(TopologicalEntry, _*) =>
			    relationshipIdentfier ! topologicalEntries
			    //send to helper actor that identifies the objects, naming if necessary, messages to parent identify objects
			  case relationships @  Seq(IdentifiedObject, _*) => {
				  addToMapMethod(relationships.asInstanceOf[Seq[IdentifiedObject]]: _*)//asInstanceOf is a cast, need to test that works correctly
				  move(randomPositiveNegative1(),randomPositiveNegative1())
			  }
			}//end react
		}//end loop
	}//end act
 }//end agent class



//import agents.QuadTree
//import agents.ObstacleJava

