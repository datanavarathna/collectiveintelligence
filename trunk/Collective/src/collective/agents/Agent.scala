package collective.agents

import scala.actors._
import Actor._
import UncertaintyMath.Measurement

case class MoveCommand(sender: Agent, x: Int, y: Int )
case class Displacement(x: Measurement, y: Measurement)
case class Move(agent: Actor, x: Measurement, y: Measurement)
case class ObjectReading(angle: Measurement, distance: Measurement)
case class TopologicalEntry(obstacle1Type: Int,obstacle2Type: Int,
                            deltaX: Measurement, deltaY: Measurement)
case class IdentifiedObject(identifier1: Int, identifier2: Int, 
                            obstacle1Type: Int,obstacle2Type: Int,
                            deltaX: Measurement, deltaY: Measurement)
case class UpdateSensor(sender: Agent)

class Agent(val environment: Actor, val topologicalElementGenerator: Actor, val relationshipIdentfier: Actor, val map: Actor) extends Actor {
    
  var relativeLocationX: Measurement = new Measurement(0,0)
  var relativeLocationY: Measurement= new Measurement(0,0)
  
  def move(x: Int,y: Int){
		environment ! MoveCommand(this,x,y)
	}

  def addToMapMethod(entries: IdentifiedObject*)
  {
    map ! entries
  }
  
	def act()
	{
		loop 
		{
			react
			{
			  case Displacement( x, y) => 
			  {
				relativeLocationX += x
				relativeLocationY += y 
				environment ! UpdateSensor(this)
			  }
			  case sensorReadings @ Seq(ObjectReading(angle,distance),_*) =>
			    topologicalElementGenerator ! sensorReadings
			    //pass to helper actor that calculates topological references and sends results as a message to parent actors
			  case topologicalEntries @ Seq(TopologicalEntry, _*) =>
			    relationshipIdentfier ! topologicalEntries
			    //send to helper actor that identifies the objects, naming if necessary, messages to parent identify objects
			  case relationships @  Seq(IdentifiedObject, _*) =>
			    addToMapMethod(relationships.asInstanceOf[Seq[IdentifiedObject]]: _*)//asInstanceOf is a cast, need to test that works correctly
			}
		}

	}
 }

case class Coordinate(x: Int, y: Int){}

class Environment( val minX: Int, val minY: Int, val maxX: Int, val maxY: Int) extends Actor{
  def this( maxX: Int, maxY: Int) = this(0,0, maxX: Int, maxY: Int)
  
  import scala.collection.mutable.Map
  
  var world = Map.empty[Actor,Coordinate]
  
  def move(x: Int, y: Int){}
  
  def act()
  {
    loop 
		{
			react
			{
			  case MoveCommand(senderAgent,x,y) => 
			  {
				  if(world.contains(senderAgent))
				  {
					  var deltaX: Measurement = new Measurement(x)
				  var deltaY: Measurement = new Measurement(y)
					  var location = world(senderAgent)
					  val oldX = location.x
					  val oldY = location.y
					  var newX = oldX
					  var newY = oldY
       
					  if(oldX + x <= maxX)
					  {
						  newX = oldX + x
					  }
					  else
					  {
						  deltaX = maxX - oldX
						  newX = maxX
					  }

					  if(oldX + x >= minX)
						  newX = oldX + x
						  else
						  {
							  deltaX = minX - oldX
							  newX = minX
						  }

					  if(oldY + y <= maxY)
						  newY = oldY + y
						  else
						  {
							  deltaY = maxY - oldY
							  newY = maxY
						  }

					  if(oldY + y >= minY)
						  newY = oldY + y
						  else
						  {
							  deltaY = minY - oldY
							  newY = minY
						  }
       
					  world - senderAgent
					  world += (senderAgent -> Coordinate(newX,newY))
					  senderAgent ! Displacement( deltaX, deltaY)
				  }
				  else
				  {
					  //throw error, should never happen
				  }//end if map contains
			  }//end case MoveCommand
			  case UpdateSensor(senderAgent) => 
			  {
				  
			  }
			}//end react
		}//end loop
  }//end act
}//end environment class
