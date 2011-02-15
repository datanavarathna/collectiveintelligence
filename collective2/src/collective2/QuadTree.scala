package collective2

class QuadTree[T] {
	import scala.collection.mutable.Map
	
	var data = Map.empty[Int,Map[Int,T]]
	
	def add(x: Int, y: Int, element: T): T = {
		var yMap = data.getOrElseUpdate(x, Map[Int,T](y -> element) )
		var returnedElement = yMap.getOrElseUpdate(y, element)
		data.update(x, yMap)
		returnedElement
	}
	
	def remove(x: Int, y: Int): Option[T] = {
		data.getOrElse(x, None ) match{
			case None => None
			case map: Map[_,_] => {
				var yMap = map.asInstanceOf[Map[Int,T]]
				var removedElement: Option[T] = yMap.remove(y)
				if(yMap.isEmpty)
					data.remove(x)
				else
					data.update(x, yMap)
				return removedElement
				
			}
			case error => throw new Exception(error+" returned no type expected")
		}
	}
	
	def remove(element: T): Option[T] = {

		var xIterator = data.keysIterator
		while(xIterator.hasNext)
		{
			var x = xIterator.next
			var yMap = data(x)
			var yIterator = yMap.keysIterator
			while(yIterator.hasNext)
			{
				var y = yIterator.next
				if( yMap(y)== element){
					var removedElement: Option[T] = yMap.remove(y)
					if(yMap.isEmpty)
						data.remove(x)
					else
						data.update(x, yMap)
					return removedElement
				}//end if found element
			}//end y while
		}//end x while
		return None
	}
	
	def apply(x: Int, y: Int): Option[T] = {
		data.get(x) match{
			case None => return None
			case Some(map: Map[_,_]) => {
				var yMap = map.asInstanceOf[Map[Int,T]]
				return yMap.get(y)
			}
			case error => throw new Exception(error+" returned no type expected")
		}//end match
	}
	
	def containsElementAt(x: Int, y: Int): Boolean = {
		apply(x,y) match {
			case None => false
			case _ => true
		}
	}
	
	def contains(element: T): Boolean = {
		for(yMap <- data.values; savedElement <- yMap; if(element == savedElement))
		{
			return true
		}
		return false
	}
	
	def range(radius: Double, x: Double, y: Double): List[T] = {
		var results: List[T] = Nil
		var xIterator = data.keysIterator
		while(xIterator.hasNext)
		{
			var mapX = xIterator.next
			var yMap = data(mapX)
			var yIterator = yMap.keysIterator
			while(yIterator.hasNext)
			{
				var mapY = yIterator.next
				var xDiff = (x-mapX)
				var yDiff = (y-mapY)
				if( math.sqrt( xDiff*xDiff + yDiff*yDiff ) <= radius)
					results = yMap(mapY) :: results
			}//end y while
		}//end x while
		return results
	}
}