package collective2
import scala.collection.mutable.BitSet

object QuadBitSet{
	def apply(): QuadBitSet = new QuadBitSet
	
	def apply(coords: (Int,Int) * ) = {
		var set = new QuadBitSet
		set.add(coords: _*)
		set
	}
}

class QuadBitSet(maxX:Int = 32) {

	import scala.collection.mutable.Map
	
	private[this] var sizeX = maxX
	
	var posXPosY = BitSet.empty
	var posXNegY = BitSet.empty
	var negXPosY = BitSet.empty
	var negXNegY = BitSet.empty
	
	def indexToXY(sizeX: Int, index: Int): (Int, Int) = {
		require (sizeX > 0)
		val x: Int = index % sizeX
        val y: Int = index / sizeX
        return (x,y)
	}
	
	def xyToIndex(x: Int, y: Int, sizeX: Int): Int = {
		require (sizeX > 0)
		return y*sizeX+x
	}
	
	def expandBitSet() = {
		var oldX = sizeX
		sizeX = 2*sizeX
		var tempPosXPosY = BitSet.empty
		var tempPosXNegY = BitSet.empty
		var tempNegXPosY = BitSet.empty
		var tempNegXNegY = BitSet.empty

		posXPosY.foreach(index => {
			val (x,y) = indexToXY(oldX,index)
			tempPosXPosY.add(xyToIndex(x,y,sizeX))
		})
		posXPosY = tempPosXPosY

		posXNegY.foreach(index => {
			val (x,y) = indexToXY(oldX,index)
			tempPosXNegY.add(xyToIndex(x,y,sizeX))
		})
		posXNegY = tempPosXNegY

		negXPosY.foreach(index => {
			val (x,y) = indexToXY(oldX,index)
			tempNegXPosY.add(xyToIndex(x,y,sizeX))
		})
		negXPosY = tempNegXPosY

		negXNegY.foreach(index => {
			val (x,y) = indexToXY(oldX,index)
			tempNegXNegY.add(xyToIndex(x,y,sizeX))
		})
		negXNegY = tempNegXNegY
	}
	
	def assignQuadrant(x: Int, y: Int): (Int,Int,BitSet) ={
		var data: BitSet = posXPosY
		var absX = x
		var absY = y
		
		if(x < 0){
			absX = -1*x
			if(y < 0){
				data = negXNegY
				absY = -1*y
			}else
				data = negXPosY
		}else if(y < 0){
			data = posXNegY
			absY = -1*y
		}
		return (absX,absY,data)
	}

	def add(x: Int, y: Int){
		
		if(x>sizeX)
			expandBitSet()
			
		var (absX,absY,data) = assignQuadrant(x,y)
		data.add(xyToIndex(absX,absY,sizeX))
	}
	
	def add(coords: (Int,Int)* ){
		for(coord <- coords){
			val (x,y) = coord
			add(x, y)
		}
	}
	
	
	def contains(x: Int, y: Int): Boolean = {
		var (absX,absY,data) = assignQuadrant(x,y)
		data(xyToIndex(absX,absY,sizeX))
	}
	
	def += (other:QuadBitSet): QuadBitSet = {
		//adds elements from this to elements of that 
		posXPosY.foreach(index => {
			val (x,y) = indexToXY(sizeX,index)
			other.add(x,y)
		})
		posXNegY.foreach(index => {
			val (x,y) = indexToXY(sizeX,index)
			other.add(x,y)
		})
		negXPosY.foreach(index => {
			val (x,y) = indexToXY(sizeX,index)
			other.add(x,y)
		})
		negXNegY.foreach(index => {
			val (x,y) = indexToXY(sizeX,index)
			other.add(x,y)
		})
		other
	}
	
	def XYs(): List[(Int,Int)] = {
		var result: List[(Int,Int)] = Nil
		posXPosY.foreach(index => {
			val (x,y) = indexToXY(sizeX,index)
			result = (x,y) :: result
		})
		posXNegY.foreach(index => {
			val (x,y) = indexToXY(sizeX,index)
			result = (x,-1*y) :: result
		})
		negXPosY.foreach(index => {
			val (x,y) = indexToXY(sizeX,index)
			result = (-1*x,y) :: result
		})
		negXNegY.foreach(index => {
			val (x,y) = indexToXY(sizeX,index)
			result = (-1*x,-1*y) :: result
		})
		return result
	}
}