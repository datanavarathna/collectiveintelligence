package collective.agents

import scala.actors._
import Actor._
import UncertaintyMath.Measurement
import agents.DataClasses

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

