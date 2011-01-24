package collective

import scala.actors._
import Actor._
import java.util.ArrayList
import uncertaintyMath.Measurement
import uncertaintyMath.Measurement._

class GoalFinder(val agent: Actor, val map: Actor) extends Actor
{
  
	def formatter(list: List[(Displacement,Int)]): List[Displacement] =
	{
		var result: List[Displacement] = Nil
		for(element <- list)
		{
		  val (vector,level) = element
		  result = vector :: result
		}
		return result
	}
  
	def breadthFirstSearch(goalType: Int, rootIdentifier: Int): List[Displacement] =
	{
		import scala.collection.mutable.Queue
		import scala.collection.mutable.Map
  
		var route: List[(Displacement,Int)] = Nil //Displacement,level
		var openQueue = new Queue[Int]//Identifier
		var identiferLevel = Map.empty[Int,Int]//identifier->level
		var globalLevel: Int = 0;
  
		openQueue.enqueue(rootIdentifier)
		identiferLevel += (rootIdentifier -> globalLevel)
		while(!openQueue.isEmpty)
		{
		  println("openQueue" + openQueue)
		  val identifier = openQueue.dequeue
		  println("After dequeue: "+openQueue)
		  val level: Int = identiferLevel.get(identifier) match{
		  	case Some(levelStored)=> levelStored
		  	case None => {println("No level found for " + identifier);0}
		  }
		  println("identifier: " + identifier + " level: " + level)
		  (map !? GetRelationsForIdentifier(identifier)) match 
		  {
              case relationsForIdentifier : Map[_,_] =>
              {
              println("Got relations " + identifier +": " + relationsForIdentifier)
              globalLevel += 1
              println("Searching level "+ globalLevel)
              val identifierRelations = relationsForIdentifier.asInstanceOf[Map[Displacement,Int]]
              var relationsIterator = identifierRelations.keysIterator
              while(relationsIterator.hasNext)
              {
                val vector = relationsIterator.next
                println("Vector: "+vector)
                identifierRelations.get(vector) match {
                  case Some(identifier)=>
                  {
                      map !? GetIdentifierType(identifier) match {
                        case IdentifierType(identifierValue,objectType)=>
                        {
                          println(identifierValue+" is of type "+objectType)
                          if(objectType == goalType)//found goal
                          {
                            println("Goal type found")
                            route = (vector,globalLevel) :: route
                            return formatter(route)
                          }
                          else if(!identiferLevel.contains(identifierValue))
                          {
                        	  println("Adding "+identifierValue+" to queue")
                        	  openQueue.enqueue(identifierValue)
                              identiferLevel += (identifierValue -> globalLevel)
                        	  println("queue: "+openQueue)
                           }
                           if(!route.isEmpty)
                           {
                        	  val (peakID, peakLevel) = route.head
                        	  println("peak: "+peakLevel+" global: "+ globalLevel+" Levels")
                        	  println("route: "+route)
                        	  if(peakLevel == globalLevel)
                        		  route = (vector,globalLevel) :: route.tail
                        	  else
                        		  route = (vector,globalLevel) :: route
                           }else
							  route = (vector,globalLevel) :: route
                           println("route updated: "+route)
                        }
                        case noType(identifier) => println("No type found for identifier "+ identifier)
                      }//end identifer type reply match
                  }//end enditifer reply
                  case _ => println("Error: Unable to find identifier for "+vector)
                }//end displacement match
                
                println("openQueue: " + openQueue)
              }//end while loop
            }//end map reply case
			case catchall => println("failed: "+ catchall)
			//case _ => println("No relations found for "+identifier)
		  }//end reply match
		}//while in queue
		println("No match found");
		return Nil
	}
  
    def act()
	{
		println("GoalFinder Running")
		loop
		{
			react
			{
              case FindGoal(obstacleType,rootIdentifier) => {
                 println("Looking for goal")
                 var foundGoal: Boolean = false
                 
                 var path = breadthFirstSearch(obstacleType,rootIdentifier)
                 if(!path.isEmpty){//return list of displacements as instructions on path to goal
                      agent ! path
                 }
                 else
                    agent ! GoalNotFound()
              }
              case "Exit" => {
                 println("Goalfinder Exiting")
                 this.exit
              }
              case catchall => println("goalfinder received: "+catchall)
			}//end react
		}//end loop
	}//end act
}