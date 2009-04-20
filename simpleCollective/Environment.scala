
import Measurement._
import scala.actors._
import Actor._

case class AgentUpdate(x: Int, y: Int, present: Boolean){
    override def toString = " x=" +x+ " y=" +y+ "present=" + present
}

class Environment( val minX: Int, val minY: Int, val maxX: Int, val maxY: Int,val scalaGui: Actor) extends Actor{
  def this( maxX: Int, maxY: Int,scalaGui: Actor) = this(0,0, maxX, maxY,scalaGui)

  import scala.collection.mutable.Map
  import scala.collection.immutable.TreeMap

  var world = Map.empty[Actor,Coordinate]
  //var obstacles = Nil //new List[Obstacle]
  //import agents.QuadTreeGateway
  var obstacles: QuadTreeGateway = new QuadTreeGateway

  def act()
  {
    println("Environment("+minX+"-"+maxX+","+minY+"-"+maxY+") running")
	loop
		{
			react
			{
			  case MoveCommand(senderAgent,x,y) =>
			  {
				  //println("MoveCommand")
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
							  if(oldX + x >= minX)
                                newX = oldX + x
                              else
                              {
							    deltaX = minX - oldX
							    newX = minX
						      }
						  }
						  else
						  {
							  deltaX = maxX - oldX
							  newX = maxX
						  }

						  if(oldY + y <= maxY)
                          {
							  if(oldY + y >= minY)
							     newY = oldY + y
							  else
							  {
								  deltaY = minY - oldY
								  newY = minY
							  }
                          }
						  else
						  {
								  deltaY = maxY - oldY
								  newY = maxY
						  }
					  }//end if target doesn't contain obstacle
					  else
					  {
						  deltaX = new Measurement(0)
						  deltaY = new Measurement(0)
					  }
                      if(newY > maxY || newY < minY || newX > maxX || newX < minX)
                      {
                          println("minX="+minX+" maxX="+maxX+" minY="+minY+" maxY="+maxY)
                          println("oldX="+oldX+" oldY="+oldY)
                          println("newX="+newX+" newY="+newY)
                          println("x="+x+" y="+y)
                      }
					  world - senderAgent
					  world += (senderAgent -> Coordinate(newX,newY))
                      scalaGui ! AgentUpdate(oldX,oldY,false)//agent left oldX,oldY
                      scalaGui ! AgentUpdate(newX,newY,true)//agent went to newX,newY
					  senderAgent ! Displacement( deltaX, deltaY)
				  }
				  else
				  {
					  println("senderAgent not recognized")//throw error, should never happen
				  }//end if map contains
			  }//end case MoveCommand
			  case UpdateSensor(senderAgent, sensorRange, sensorDeltaAngle, sensorDeltaRange) =>
			  {
				   //println("UpdateSensor")
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
                     else
                        println("senderAgent not recognized")
			  }
			  case obstacleList @ List(Obstacle(_,_,_), _*) =>
			  {
				println("Received obstacleList")
                for(obstacle <- obstacleList.asInstanceOf[List[Obstacle]])
                {
                    obstacles.add(obstacle)
                    println("Added " + obstacle)
                }
			  }
			  case agentListWithLocation @ List(AgentWithLocation(_,_,_), _*) =>
			  {
				println("Received agentListWithLocation")
                for(AgentWithLocation(agent,x,y) <- agentListWithLocation.asInstanceOf[List[AgentWithLocation]])
                {
                    world += (agent -> Coordinate(x,y))
                    println("Added following to world: " + agent + " x="+x+" y="+y)
                    agent.start
                }
			  }
              case "Exit" => {
                   for(agent <- world.keySet){
                       agent ! "Exit"
                   }
                   scalaGui ! "Exit"
                   this.exit
              }
              case catchAll => println("Catchall: " +catchAll)
			}//end react
		}//end loop
  }//end act
}//end environment class