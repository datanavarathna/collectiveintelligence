package collective.environment

import scala.collection.mutable.Map
import scala.actors._
import Actor._

case class Coordinate(x: Int, y: Int){}

class Environment( val minX: Int, val minY: Int, val maxX: Int, val maxY: Int) extends Actor{
  def this( maxX: Int, maxY: Int) = this(0,0, maxX: Int, maxY: Int)
  
  var world = new Map[Actor,Coordinate]
  
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
			        location = world(senderAgent)
			        if(location.x + x <= maxX)
			        	location.x
			        else
			        	location.x = maxX
			      }
			      else
			      {
			        //throw error, should never happen
			      }
			    }
			}
		}
  }
}
