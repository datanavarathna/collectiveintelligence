package collective2

trait TrackAdd[T] extends QuadTree[T] {
	private[this] var sizeIncreasedBoolean = false
	
	override def add(x: Int, y: Int, element: T): T = {
		sizeIncreasedBoolean = true
		super.add(x,y,element)
	}
	
	def sizeIncreased: Boolean = sizeIncreasedBoolean
	
	def resetSizeIncreased(){
		sizeIncreasedBoolean = false
	}
}