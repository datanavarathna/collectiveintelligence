package collective2

import uncertaintyMath.Measurement
import uncertaintyMath.Measurement._
import scala.actors._
import Actor._

class Environment( val minX: Int, val minY: Int, val maxX: Int, val maxY: Int,val scalaGui: Actor) extends Actor{
  def this( maxX: Int, maxY: Int,scalaGui: Actor) = this(0,0, maxX, maxY,scalaGui)

  
  import scala.collection.mutable.Map
  import scala.collection.immutable.TreeMap

  private var world = Map.empty[Actor,Coordinate]//key,value
  //var obstacles = Nil //new List[Obstacle]
  //import agents.QuadTreeGateway
  private var obstacles = new QuadTree[Obstacle]

  private var goalObstacle: Obstacle = _
  private var goalObstacleSet: Boolean = false

  private val epsilon = 0.001
  
  def dimensions: (Int,Int) = {
	  (maxX-minX,maxY-minY)
  }
  
  def act()
  {
	//self.trapExit = true//receives termination notification in mailbox
	link(scalaGui)
    println("Environment("+minX+"-"+maxX+","+minY+"-"+maxY+") running")
	loop
		{
			react
			{
			  case 'Dimensions => reply( dimensions )
				
			  case MoveCommand(senderAgent,x,y) =>
			  {
				  
                  if(world.contains(senderAgent))
				  {
					  var deltaX: Measurement = new Measurement(x,epsilon)
				      var deltaY: Measurement = new Measurement(y,epsilon)
					  var location = world(senderAgent)
					  val oldX = location.x
					  val oldY = location.y
					  var newX = oldX
					  var newY = oldY
                      
                      //println("MoveCommand(" +(oldX + x)+","+(oldY + y)+")")
					  //determine if an agent occupies the space that the senderAgent wants to move to
                      var agentAtTarget: Boolean = false
                      val agentIterator = world.valuesIterator
                      while (agentIterator.hasNext && !agentAtTarget)
                      {
                          val agentWorldCoord: Coordinate = agentIterator.next
                          //println("agentWorldCoord: " + agentWorldCoord)
                          agentAtTarget = (agentWorldCoord == Coordinate(oldX + x,oldY + y)) || agentAtTarget
                      }
                      //println("agentAtTarget: " + agentAtTarget)
					  if(!obstacles.containsElementAt(oldX + x,oldY + y) && !agentAtTarget)//if target doesn't contain obstacle
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
						  deltaX = new Measurement(0,epsilon)
						  deltaY = new Measurement(0,epsilon)
                          
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
					  reply( Displacement( deltaX, deltaY) )
                      /*
                      if(deltaX != new Measurement(newX-oldX))
                        println(deltaX+"="+newX + "-" + oldX + "false")
                      if(deltaY != new Measurement(newY-oldY))
                        println(deltaY+"="+newY + "-" + oldY + "false")
                      */
                      if(deltaX != new Measurement(0) && deltaY != new Measurement(0)){//if agent moved
                        /*
                        println(deltaX+"="+ new Measurement(newX-oldX))
                        println(deltaY+"="+ new Measurement(newY-oldY))
                        */
                        //println("Moved to ("+newX+","+newY+")")
                        world(senderAgent) = Coordinate(newX,newY)//update agent location in world map
                        //world - senderAgent
                        //world += (senderAgent -> Coordinate(newX,newY))
                        scalaGui ! AgentUpdate(oldX,oldY,newX,newY)
                      }
				  }
				  else
				  {
					  throw new Exception(senderAgent+" senderAgent not recognized")//should never happen
				  }//end if map contains
			  }//end case MoveCommand
			  case UpdateSensor(senderAgent, sensorRange, sensorDeltaAngle, sensorDeltaRange) =>
			  {
				   //println("UpdateSensor")
					 
					 if(world.contains(senderAgent))
					 {
						 val agentCoordinate: Coordinate = world(senderAgent)
                         //println("Obstacles:" + obstacles)
                         val obstaclesInRange = obstacles.range(sensorRange, agentCoordinate.x, agentCoordinate.y)
                         //println("sensorRange: " + sensorRange)
                         //println("Obstacles in range: " + obstaclesInRange)
						 var detectedObstacles: List[ObjectReading] = Nil
						 for {obstacle: Obstacle <- obstaclesInRange}
						 {
                            //println("detectedObstacle: " + obstacle)
							//create coordinates relative distances from agent
						    val vectorX: Double = obstacle.x - agentCoordinate.x
						    val vectorY: Double = obstacle.y - agentCoordinate.y
						  	val angle = new Measurement(math.atan2(vectorX, vectorY),sensorDeltaAngle*math.Pi/180)
						    val distance = new Measurement(math.sqrt(vectorX*vectorX + vectorY*vectorY),sensorDeltaRange)
						    val objectReading = ObjectReading(angle, distance, obstacle.obstacleType)
                            //println("sent objectReading" + objectReading)
                            detectedObstacles= objectReading :: detectedObstacles
						 }
						 //computer scanned space
						 val scannedArea = new QuadBitSet
						 val range = sensorRange.toInt
						 for(x <- 0 to range; y <- 0 to range; if(math.sqrt(x*x+y*y) <= range)){
							scannedArea.add(x,y) 
						 }

                         var detectedAgents: List[AgentReading] = Nil
                         val agentIterator = world.keysIterator
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
                                     val vectorX: Int = coordinate.x - agentCoordinate.x
                                     val vectorY: Int = coordinate.y - agentCoordinate.y
                                     //println("agentInMap: (" + coordinate.x + "," + coordinate.y + ")")
                                     if(vectorX != 0 || vectorY != 0)
                                     {
                                         val distance = math.sqrt(vectorX*vectorX + vectorY*vectorY)
                                         if(distance < sensorRange)
                                         {
                                            val angle = new Measurement(math.atan2(vectorY, vectorX))
                                            val distanceMeasurement = new Measurement(distance)
                                            val agentReading = AgentReading(angle, distance)
                                            detectedAgents = agentReading :: detectedAgents
                                            //println("("+vectorX+","+vectorY+")")
                                            //println("environment agent reading: " + agentReading)
                                        } //end if in sensor range
                                     }//end if not self
                                     
                                 }
                                 case None => {
                                      throw new Exception("Error: Value not found for key " + agentInMap + "in map")
                                 }
                             }//end match
                             
                         }//end while
						 
                         val scan = Scan(scannedArea,detectedObstacles,detectedAgents)
                         reply ( scan )
                         //println("Environment sending: " + scan)
					 }
                     else
                        throw new Exception(senderAgent+" senderAgent not recognized")
			  }
			  //from scalaGUI 
			  case listOfObstacles @ List(Obstacle(_,_,_), _*) =>
			  {
			 	println("Received obstacleList")
			 	val obstacleList = listOfObstacles.asInstanceOf[List[Obstacle]]
                for(obstacle <- obstacleList)
                {
                	println("Processing "+obstacle)
                    if(obstacle.obstacleType > 1){
                        goalObstacle = obstacle
                        goalObstacleSet = true
                    }
                    val obstacleX = obstacle.x.toInt
                    val obstacleY = obstacle.y.toInt
                    obstacles.add(obstacleX,obstacleY,obstacle)
                    println("Completed obstacles.add operation")
                    if( obstacles(obstacleX,obstacleY)== Some(obstacle))//the QuadTree apply method is hanging
                        println("Added " + obstacle)
                    else
                        println("Failed to add " + obstacle)
                }
                println("Obstacles: " + obstacles)
			  }//end case obstacleList
			   //from scalaGUI
			  case agentListWithLocation @ List(AgentWithLocation(_,_,_), _*) =>
			  {
				println("Received agentListWithLocation")
                for(AgentWithLocation(agent,x,y) <- agentListWithLocation.asInstanceOf[List[AgentWithLocation]])
                {
                    world += (agent -> Coordinate(x,y))
                    println("Added following to world: " + agent + " x="+x+" y="+y)
                    agent.start
                    if(goalObstacleSet)
                        agent ! Goal(goalObstacle)
                }
                println("Agents in World: " + world)
                //send "Start" message to every agent in the world
               world.foreach( pair => {
            	   val (agent,_) = pair
            	   agent ! "Start" 
            	   }
               )//end foreach
               /*
                val agentsIterator = world.keysIterator
                while(agentsIterator.hasNext)
                {
                    agentsIterator.next ! "Start"
                }
                */
			  }//end case agentList
			  
			   /*
              case "Exit" => {
                   println("Environment Exiting")
                   for(agent <- world.keySet){
                       agent ! "Exit"
                   }
                   scalaGui ! "Exit"
                   this.exit
              }*/
			   /*
			  case Exit(from,reason) => {
			 	  println("Environment exiting")
			  }
			  */
              case catchAll => println("Environment Catchall: " +catchAll)
			}//end react
		}//end loop
  }//end act
}//end environment class