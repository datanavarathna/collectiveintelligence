package focusedDstar
import focusedDstar._
import scala.collection.mutable

case class CoordinateState(val x: Int, val y: Int, var passable: Boolean = true,
		factory: StateConstructor) extends State(factory){
	
	override def toString = "CoordinateState("+x+","+y+","+"passable="+passable+")"
	
}

class CoordinateCreator(minXY: (Int,Int), maxXY: (Int,Int)) extends StateConstructor{
	val (minX,minY)= minXY
	val (maxX,maxY)= maxXY
	var coordinates = mutable.WeakHashMap.empty[(Int,Int),CoordinateState]
	
	def withinBounds(x: Int, y: Int): Boolean = {
		minX <= x && x <= maxX &&  minY <= y && y <= maxY
	}
		
	def getNeighbors(state: State): Seq[State] = {
		state match{
			case CoordinateState(x,y,_,_) =>{
				for (horiz <- (x-1) to (x+1);vert <- (y-1) to (y+1);
					if(!(horiz == x && vert == y ) && withinBounds(horiz,vert) ) ) 
					yield{
						//println("Coord("+x+","+y+")Getting neighbor horiz= "+horiz+" vert= "+vert)	
						getCoordinate(horiz,vert)
					}
			}
			case that => {
				throw new Exception(that+" is an incompatable State")
			}
		}
	}
	
	
	def createCoordinate(x: Int, y: Int, passable: Boolean = true): CoordinateState = {
		//println("Creating coordinate ("+x+","+y+")")
		val newCoordinate = new CoordinateState(x,y,passable, this)
		coordinates += ((x,y)-> newCoordinate)
		newCoordinate
	}
	
	def getCoordinate(x: Int, y: Int): CoordinateState = {
		if(withinBounds(x,y))
			coordinates.getOrElseUpdate((x,y),		
			createCoordinate(x,y) )
		else
			null
	}
		
	
	override def toString: String = coordinates.values.toString
}

trait CartesianCoordinateOneUnitDiagonalDStar extends focusedDstar {

	private var passabilityMap = mutable.Map.empty[(CoordinateState,CoordinateState),Double]
	
	def sensor: Map[(State,State),Double] 
	
	def costOfTransversal(x: State, y: State): Double = {
		(x,y) match {
			case (a: CoordinateState, b: CoordinateState) => {
				passabilityMap.getOrElse((a,b),
						if(a.passable && b.passable )
							math.max(math.abs(a.x-b.x), math.abs(a.y-b.y))
						else{
							updateCostOfTransversal(a,b,Double.PositiveInfinity)
							Double.PositiveInfinity
						}
				)
			}
			
			case _ => throw new Exception(x+" and "+y+" are not CoordinateState")
		}//end match
	}
	
	def updateCostOfTransversal(x: State, y: State, costValue: Double) = {
		(x,y) match {
			case (a: CoordinateState, b: CoordinateState) => {
				passabilityMap +=((a,b) -> costValue)
			}
			
			case _ => throw new Exception(x+" and "+y+" are not CoordinateStates")
		}//end match
	}
}