package collective.agents

//import agents.Obstacle

class QuadTree {
  var obstacles = Nil //new List[Obstacle]
  
  def contains(x: Int,y: Int):Boolean =
  {
		  true
  }
  def add(x: Int,y: Int):Boolean  = //returns true if successful
  {
	  true
  }
  def remove(x: Int,y: Int):Boolean = //returns true if successful
  {
	  true
  }
  def range(sensorRange: Int, x: Int, y: Int):List[Obstacle] = 
  {
		  for (
				  obstacle: Obstacle <- obstacles
				  if(obstacle.x*obstacle.x* + obstacle.y*obstacle.y <= sensorRange)
		  ) yield {
			  obstacle
		  }

  }

	


}
