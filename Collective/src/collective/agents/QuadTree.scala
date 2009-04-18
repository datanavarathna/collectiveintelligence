package collective.agents



abstract class QuadTree {
  var obstacles = Nil //new List[Obstacle]
  
  def contains(x: Int,y: Int):Boolean
 
	def add(x: Int,y: Int):Boolean //returns true if successful
 
	def remove(x: Int,y: Int):Boolean //returns true if successful
 
	def range(sensorRange: Int, maxX: Int, maxY: Int):List[Obstacle] = 
	{
			for (
					obstacle: Obstacle <- obstacles
					if(obstacle.x*obstacle.x* + obstacle.y*obstacle.y <= sensorRange)
			) yield {
				obstacle
			}
			
	}

	


}
