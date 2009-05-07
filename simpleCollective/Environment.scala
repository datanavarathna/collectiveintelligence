
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

  private var world = Map.empty[Actor,Coordinate]//key,value
  //var obstacles = Nil //new List[Obstacle]
  //import agents.QuadTreeGateway
  private var obstacles: QuadTreeGateway = new QuadTreeGateway

  def act()
  {
    println("Environment("+minX+"-"+maxX+","+minY+"-"+maxY+") running")
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
                      
                      //println("MoveCommand(" +(oldX + x)+","+(oldY + y)+")")
                      var agentAtTarget: Boolean = false
                      val agentIterator = world.values
                      while (agentIterator.hasNext)
                      {
                          val agentWorldCoord: Coordinate = agentIterator.next
                          //println("agentWorldCoord: " + agentWorldCoord)
                          agentAtTarget = (agentWorldCoord == Coordinate(oldX + x,oldY + y)) || agentAtTarget
                      }
                      //println("agentAtTarget: " + agentAtTarget)
					  if(!obstacles.contains(oldX + x,oldY + y) && !agentAtTarget)//if target doesn't contain obstacle
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
                          
                          //println("Obstacle at (" +(oldX + x)+ "," +(oldY + y)+ ")")
                          //println("Agent was at ("+oldX+","+oldY +")")
                          //println("Agent moved to ("+newX+" ,"+newY+")")
                          
					  }
                      //trying to go off the edge
                      if(newY > maxY || newY < minY || newX > maxX || newX < minX)
                      {
                          println("minX="+minX+" maxX="+maxX+" minY="+minY+" maxY="+maxY)
                          println("oldX="+oldX+" oldY="+oldY)
                          println("newX="+newX+" newY="+newY)
                          println("x="+x+" y="+y)
                      }
					  senderAgent ! Displacement( deltaX, deltaY)
                      if(deltaX != 0 && deltaY != 0){
                        println("Moved to ("+newX+","+newY+")")
                        world - senderAgent
                        world += (senderAgent -> Coordinate(newX,newY))
                        scalaGui ! AgentUpdate(oldX,oldY,false)//agent left oldX,oldY
                        scalaGui ! AgentUpdate(newX,newY,true)//agent went to newX,newY
                      }
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
                         //println("Obstacles:" + obstacles)
                         val obstaclesInRange = obstacles.range(sensorRange, agent.x, agent.y)
                         //println("sensorRange: " + sensorRange)
                         //println("Obstacles in range: " + obstaclesInRange)
						 var detectedObstacles: List[ObjectReading] = Nil
						 for {obstacle: Obstacle <- obstaclesInRange}
						 {
                            //println("detectedObstacle: " + obstacle)
						    val vectorX: Int = obstacle.x - agent.x
						    val vectorY: Int = obstacle.y - agent.y
						  	val angle = new Measurement(Math.atan2(vectorX, vectorY))
						    val distance = new Measurement(Math.sqrt(vectorX*vectorX + vectorY*vectorY))
						    val objectReading = ObjectReading(angle, distance, obstacle.obstacleType)
                            //println("sent objectReading" + objectReading)
                            detectedObstacles= objectReading :: detectedObstacles
						 }
						 senderAgent ! detectedObstacles
                         //println("Environment sending: " + detectedObstacles)

                         var detectedAgents: List[AgentReading] = Nil
                         val agentIterator = world.keys
                         var counter: Int = 0
                         //println("request agent: " + agent)
                         while (agentIterator.hasNext)
                         {
                             val agentInMap = agentIterator.next
                             //println("agentInMap: " + agentInMap)
                             counter += 1
                             //println("World agent: " + counter)
                             world.get(agentInMap) match {
                                 case Some(coordinate) => {
                                     val vectorX: Int = coordinate.x - agent.x
                                     val vectorY: Int = coordinate.y - agent.y
                                     //println("agentInMap: (" + coordinate.x + "," + coordinate.y + ")")
                                     if(vectorX != 0 || vectorY != 0)
                                     {
                                         val distance = Math.sqrt(vectorX*vectorX + vectorY*vectorY)
                                         if(distance < sensorRange)
                                         {
                                            val angle = new Measurement(Math.atan2(vectorY, vectorX))
                                            val distanceMeasurement = new Measurement(distance)
                                            val agentReading = AgentReading(angle, distance)
                                            detectedAgents = agentReading :: detectedAgents
                                            //println("("+vectorX+","+vectorY+")")
                                            //println("environment agent reading: " + agentReading)
                                        } //end if in sensor range
                                     }//end if not self
                                     
                                 }
                                 case None => {
                                      println("Error: Value not found for key " + agentInMap + "in map")
                                 }
                             }//end match
                             
                         }//end while
                         senderAgent ! detectedAgents
                         /*
                         if(!detectedAgents.isEmpty)
                             println("Environment sending: " + detectedAgents)
                         */
					 }
                     else
                        println("senderAgent not recognized")
			  }
			  case obstacleList @ List(Obstacle(_,_,_), _*) =>
			  {
				println("Received obstacleList")
                for(obstacle <- obstacleList.asInstanceOf[List[Obstacle]])
                {
                    if(obstacles.add(obstacle))
                        println("Added " + obstacle)
                    else
                        println("Failed to add " + obstacle)
                }
                println("Obstacles: " + obstacles)
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
                println("Agents in World: " + world)
                val agentsIterator = world.keys
                while(agentsIterator.hasNext)
                {
                    agentsIterator.next ! "Start"
                }
			  }
              case "Exit" => {
                   println("Environment Exiting")
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