package collective.agents

import scala.actors._
import Actor._
import UncertaintyMath.Measurement
//import agents.DataClasses

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
case class Obstacle(obstacleType: Int, x: Int, y: Int) 

class Agent(val environment: Actor, val topologicalElementGenerator: Actor, val relationshipIdentfier: Actor, val map: Actor,
            val sensorRange: Int, val sensorDeltaAngle: Int, val SensorDeltaRange: Int) extends Actor 
{
    
  var relativeLocationX: Measurement = new Measurement(0,0)
  var relativeLocationY: Measurement = new Measurement(0,0)
  val randomGenerator: Random = new Random
  
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

import collective.agents._
import UncertaintyMath.Measurement
import scala.actors._
import Actor._

class Environment( val minX: Int, val minY: Int, val maxX: Int, val maxY: Int) extends Actor{
  def this( maxX: Int, maxY: Int) = this(0,0, maxX: Int, maxY: Int)
  
  import scala.collection.mutable.Map
  import scala.collection.immutable.TreeMap
  
  var world = Map.empty[Actor,Coordinate]
  //var obstacles = Nil //new List[Obstacle]
  import agents.QuadTreeGateway
  var obstacles: QuadTreeGateway = new QuadTreeGateway
  
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

					  if(!obstacles.contains(x,y))//if target doesn't contain obstacle
					  {
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
					  }//end if target doesn't contain obstacle
					  else
					  {
						  deltaX = new Measurement(0)
						  deltaY = new Measurement(0)
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
			  case UpdateSensor(senderAgent, sensorRange, sensorDeltaAngle, sensorDeltaRange) => 
			  {
				   
					 /*			 
					 for (
							 obstacle: Obstacle <- obstacles
							 if(obstacle.x*obstacle.x* + obstacle.y*obstacle.y <= sensorRange)
					 ) yield 
					 {

						 if(world.contains(senderAgent))
						 {
							 val obstacleX = obstacle.x
							 val obstacleY = obstacle.y 
							 val agent = world(senderAgent)
							 val vectorX = obstacleX - agent.x
							 val vectorY = obstacleY - agent.y
							 val angle = new Measurement(Math.atan2(vectorX, vectorY))
							 val distance = new Measurement(Math.sqrt(vectorX*vectorX + vectorY*vectorY))
							 ObjectReading(angle, distance)
						 }
						 else
						 {
							 //throw error, should never happern
						 }


					 }//end for
					 */
					 if(world.contains(senderAgent))
					 {
						 val agent = world(senderAgent)
						 val detectedObstacles =					 
							 for {obstacle: Obstacle <- obstacles.range(sensorRange, agent.x, agent.y)}
						     {
						    	 val vectorX = obstacle.x - agent.x
						    	 val vectorY = obstacle.y - agent.y
						    	 val angle = new Measurement(Math.atan2(vectorX, vectorY))
						    	 val distance = new Measurement(Math.sqrt(vectorX*vectorX + vectorY*vectorY))
						    	 ObjectReading(angle, distance)
						     }
						 senderAgent ! detectedObstacles
					 }
			  }
			}//end react
		}//end loop
  }//end act
}//end environment class
