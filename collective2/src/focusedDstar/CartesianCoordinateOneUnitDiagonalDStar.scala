package focusedDstar
import focusedDstar._
import scala.collection.mutable

object CoordinateState{
	var numOfStates: Int = 0
	
	def newState(): Int = {
		numOfStates += 1
		numOfStates
	}
}

case class CoordinateState(val x: Int, val y: Int, var passable: Boolean = true,
		factory: StateConstructor) extends State(factory){
	val stateID = CoordinateState.newState
	override def toString = "CoordinateState("+x+","+y+",id= "+stateID+
		","+"passable="+passable+","+super.toString+")"
	
	override def hashCode: Int = {
		val factoryHashCode = if(factory == null) 0 else factory.hashCode
		41*(41*(41+factoryHashCode)+x.hashCode)+y.hashCode
	}
	
	//override def compare (x: State) : Int = super.compare(x)
}

class CoordinateCreator(minXY: (Int,Int), maxXY: (Int,Int)) extends StateConstructor{
	private[this] val (minX,minY)= minXY
	private[this] val (maxX,maxY)= maxXY
	private[this] var coordinates = mutable.HashMap.empty[(Int,Int),CoordinateState]
	
	println("CoordinateCreator( ("+minX+","+minY+"), ("+maxX+","+maxY+") )")
	
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
		if(withinBounds(x,y)){
			coordinates.getOrElseUpdate((x,y),		
			createCoordinate(x,y) )
		}else
			null
	}
		
	
	override def toString: String = coordinates.values.toString
}

trait CartesianCoordinateOneUnitDiagonalDStar extends focusedDstar {
	
	private[this] var passabilityMap = mutable.Map.empty[(CoordinateState,CoordinateState),Double]
	
	def passabilitySize = passabilityMap.size
	
	def sensor: Map[(State,State),Double] 
	
	def costOfTransversal(x: State, y: State): Double = {
		try{
		(x,y) match {
			case (a: CoordinateState, b: CoordinateState) => {//problems occur because of changing hash values
				val result = passabilityMap.getOrElse((a,b),
						if(a.passable && b.passable ){
							val aX = a.x 
							val aY = a.y 
							val bX = b.x 
							val bY = b.y 
							val diffX = math.abs(aX-bX)
							val diffY = math.abs(aY-bY)
							var cost: Double = math.max(diffX, diffY)
							if(diffX > 0 && diffY > 0)
								cost = cost + 0.0001
							cost
						}	
						else{
							updateCostOfTransversal(a,b,obstacleCost)
							obstacleCost
						}
				)
				//if((a.x == 0 && a.y == -1) || (b.x == 0 && b.y == -1) )
					//println("cost ("+a.x+","+a.y+")->("+b.x+","+b.y+")= "+result )
				result
			}
			
			case _ => throw new Exception(x+" and "+y+" are not CoordinateState")
		}//end match
		}catch{
			case e: Exception => {
				println("Exception: "+e)
				Double.PositiveInfinity
			}
		}
	}
	
	def updateCostOfTransversal(x: State, y: State, costValue: Double) = {
		(x,y) match {
			case (a: CoordinateState, b: CoordinateState) => {
				passabilityMap +=((a,b) -> costValue)
				//passabilityMap +=((b,a) -> costValue)
				//if((a.x == 0 && a.y == -1) || (b.x == 0 && b.y == -1) ){
					//println("update("+a.x+","+a.y+")->("+b.x+","+b.y+")= "+costValue )
					//println("update("+b.x+","+b.y+")->("+a.x+","+a.y+")= "+costValue )
				//}
			}
			
			case _ => throw new Exception(x+" and "+y+" are not CoordinateStates")
		}//end match
	}
}