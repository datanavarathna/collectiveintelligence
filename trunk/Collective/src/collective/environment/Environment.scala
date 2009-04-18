package collective.environment

import agents.*
import UncertaintyMath.Measurement

class Environment( val minX: Int, val minY: Int, val maxX: Int, val maxY: Int) extends Actor{
  def this( maxX: Int, maxY: Int) = this(0,0, maxX: Int, maxY: Int)
  
  import scala.collection.mutable.Map
  import scala.collection.immutable.TreeMap
  
  var world = Map.empty[Actor,Coordinate]
  //var obstacles = Nil //new List[Obstacle]
  var obstacles: QuadTree = new QuadTree
  
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
							 for {obstacle: Obstacle <- obstacles.range(sensorRange, x: Int, y: Int)}
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