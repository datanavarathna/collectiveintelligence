package Agents

import scala.actors._
import Actor._
import UncertaintyMath.Measurement

case class MoveCommand(sender: Agent, x: Int, y: Int )
case class Displacement(x: Measurement, y: Measurement)
case class Move(agent: Actor, x: Measurement, y: Measurement)
case class ObjectReading(angle: Measurement, distance: Measurement)
case class TopologicalEntry(obstacle1Type: Int,obstacle2Type: Int,
                            deltaX: Measurement, deltaY: Measurement)
case class identifiedObject(identifier1: Int, identifier2: Int, 
                            obstacle1Type: Int,obstacle2Type: Int,
                            deltaX: Measurement, deltaY: Measurement)


class Agent(val environment: Actor) extends Actor {
	val topologicalElementGenerator: Actor
    val relationshipIdentfier: Actor
    val map: Actor
    
    val relativeLocationX: Measurement
    val relativeLocationY: Measurement
  
  def move(x: Int,y: Int){
		environment ! MoveCommand(this,x,y)
	}

  def addToMapMethod(entries: identifiedObject*)
  {
    map ! entries
  }
  
	def act()
	{
		loop 
		{
			react
			{
			  case Displacement( x, y) => {
				relativeLocationX += x
				relativeLocationY += y }
			  case sensorReadings @ Seq(ObjectReading(angle,distance),_*) =>
			    topologicalElementGenerator ! sensorReadings
			    //pass to helper actor that calculates topological references and sends results as a message to parent actors
			  case topologicalEntries @ Seq(TopologicalEntry(object1Type,object2Type,x,y),_*) =>
			    relationshipIdentfier ! topologicalEntries
			    //send to helper actor that identifies the objects, naming if necessary, messages to parent identify objects
			  case relationships @ Seq(identifiedObject(identifier1,identfier2,object1Type,object2Type,x,y)) =>
			    addToMapMethod(relationships)
			}
		}

	}
 }

