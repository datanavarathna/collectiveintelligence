package focusedDstar
import focusedDstar._
import scala.collection.mutable

case class Coordinate(val x: Int, val y: Int, var passable: Boolean = true,
		adjacent: Seq[State] = Stream.empty) extends State(adjacent){
	
}

class CoordinateCreator(minXY: (Int,Int), maxXY: (Int,Int)) {
	val (minX,minY)= minXY
	val (maxX,maxY)= maxXY
	var coordinates = mutable.Map.empty[(Int,Int),Coordinate]
	
	private def withinBounds(x: Int, y: Int): Boolean = {
		minX <= x && x <= maxX &&  minY <= y && y <= maxY
	}
		
	
	def createCoordinate(x: Int, y: Int, passable: Boolean = true): Coordinate = {
		//println("Creating coordinate ("+x+","+y+")")
		val newCoordinate = new Coordinate(x,y,passable)
		coordinates += ((x,y)-> newCoordinate)
		lazy val neighbors: Seq[Coordinate] = {
				for (horiz <- (x-1) to (x+1);vert <- (y-1) to (y+1);
					if(!(horiz == x && vert == y ) && withinBounds(horiz,vert) ) ) 
					yield{
						//println("Getting neighbor horiz= "+horiz+" vert= "+vert)	
						getCoordinate(horiz,vert)
					}
		}//end neighbors
		newCoordinate.neighbors_(neighbors)
		newCoordinate
	}
	
	def getCoordinate(x: Int, y: Int): Coordinate = {
		if(withinBounds(x,y))
			coordinates.getOrElseUpdate((x,y),		
			createCoordinate(x,y) )
		else
			null
	}
		
	
	override def toString: String = coordinates.values.toString
}

abstract class CartesianCoordinateOneUnitDiagonalDStar extends focusedDstar {

	private var impassableMap = mutable.Map.empty[(Coordinate,Coordinate),Double]
	
	def sensor: Map[(State,State),Double] 
	
	def transitionToState(next: State): State
	
	def costOfTransversal(x: State, y: State): Double = {
		(x,y) match {
			case (a: Coordinate, b: Coordinate) => {
				impassableMap.getOrElse((a,b),
						if(a.passable && b.passable )
							math.max(math.abs(a.x-b.x), math.abs(a.y-b.y))
						else{
							updateCostOfTransversal(a,b,Double.PositiveInfinity)
							Double.PositiveInfinity
						}
				)
			}
			
			case _ => throw new Exception(x+" and "+y+" are not Coordinate States")
		}//end match
	}
	
	def updateCostOfTransversal(x: State, y: State, costValue: Double) = {
		(x,y) match {
			case (a: Coordinate, b: Coordinate) => {
				impassableMap +=((a,b) -> costValue)
			}
			
			case _ => throw new Exception(x+" and "+y+" are not Coordinate States")
		}//end match
	}
}