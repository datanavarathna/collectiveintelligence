class QuadTreeGateway
{
	private val qT = new QuadTree()

    def add(o: Obstacle): Boolean =
	{
			val o2 = new ObstacleJava();
			o2.x = o.x;
			o2.y = o.y;
			o2.Otype = o.obstacleType;
			qT.add(o2).asInstanceOf[Boolean]
	}

	def add(obstacleType: Int, x: Int, y: Int): Boolean =
	{
			val o2 = new ObstacleJava()
			o2.Otype = obstacleType
			o2.x = x
			o2.y = y
			qT.add(o2).asInstanceOf[Boolean]
	}

	def remove(o: Obstacle): Boolean =
	{
			val o2 = new ObstacleJava()
			o2.x = o.x
			o2.y = o.y
			o2.Otype = o.obstacleType
			val outp = qT.remove(o2).asInstanceOf[Boolean]
			outp
	}

	def remove(obstacleType: Int, x: Int, y: Int): Boolean =
	{
			val o2 = new ObstacleJava()
			o2.Otype = obstacleType
			o2.x = x
			o2.y = y
			o2.Otype = obstacleType
			val outp = qT.remove(o2).asInstanceOf[Boolean]
			outp
	}

	def contains(o: Obstacle): Boolean =
	{
			val o2 = new ObstacleJava()
			o2.x = o.x
			o2.y = o.y
			o2.Otype = o.obstacleType
			val outp = qT.contains(o2)
			outp
	}

	def contains( x: Int, y: Int): Boolean =
	{
			val o2 = new ObstacleJava()
			o2.x = x
			o2.y = y
			val outp = qT.contains(o2).asInstanceOf[Boolean]
			outp
	}

	def range(radius: Double, x: Double, y: Double): List[Obstacle] =
	{
		val input = qT.returnAll()
		var output = Nil.asInstanceOf[List[Obstacle]]
		for (i <- 0 until input.length)
		{
			val obst: ObstacleJava = input(i).asInstanceOf[ObstacleJava]
			if(Math.sqrt((x-obst.x)*(x-obst.x)+(y-obst.y)*(y-obst.y))<=radius){
				val obst2 = Obstacle(obst.Otype,obst.x,obst.y);
				output = obst2::output
			}
		}
		output
	}
}
