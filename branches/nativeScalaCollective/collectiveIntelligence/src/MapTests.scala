import scala.actors._
import Actor._
import Measurement._

object MapTests {
    
  def PolarToCartesian (distance: Measurement,angle: Measurement): Displacement =  {
      Displacement(distance * cos(angle), distance * sin(angle))
    }
  
    def relationGenerator(readingA: InternalIdentifiedObject, readingB: InternalIdentifiedObject): Relationship = {
        val abVector: Displacement = readingB.vectorFromAgent - readingA.vectorFromAgent
        new Relationship(readingA.name, readingB.name, abVector)
    }
  
  def sensorProcessing(cartesianReadings: List[InternalIdentifiedObject], sensorReadings: List[ObjectReading]): List[Relationship] =
  {
		println("Processing sensor readings")

		var entries: List[Relationship] = Nil
		var readings: List[InternalIdentifiedObject]  = cartesianReadings
		while(!readings.isEmpty){
			val readingA = readings.head
            readings = readings.tail
            for(readingB <- readings){
            	//println("readings: "+readingA+", "+readingB)
            	entries = relationGenerator(readingA,readingB) :: entries
            }
        }//end while
        println("Relationships from readings: " + entries)
        println("cartesianReadings: " + cartesianReadings)
        //println("readings: " + readings)
        entries
					  
    }//end case
  
	def main(args : Array[String]) = 
   {
     var collectiveMap = new CollectiveMap
     
		val agent = actor {
			loop {
				println("Running")
				react {
				  case NewIdentifiedObjects(lastUpdate,newIdentifiedObjects,cartesianObjectReadings, sensorRange) => {
					  println("Adding  the following relationships to CollectiveMap: " + newIdentifiedObjects)
				  	collectiveMap ! Add(lastUpdate, newIdentifiedObjects, cartesianObjectReadings, sensorRange)
				  	collectiveMap ! MapSize
				  }
				  case msg => println("received message: "+ msg)
				}//end receive
			}//end while
		}//end actor
     
     var sensorRange: Double = 10
     var goalFinder = new GoalFinder( agent, collectiveMap)
     var sensorProcessor = new SensorObjectReadingsProcessor(collectiveMap,agent)
     collectiveMap.start
     goalFinder.start
     sensorProcessor.start
     //agent.start
     
     Thread.sleep(300)//msec
     var updateTime: Long = System.currentTimeMillis();
     
     var identifiedObjects: List[IdentifiedObject] = Nil
     var cartesianReadings: List[InternalIdentifiedObject] = Nil
     var sensorReadings: List[ObjectReading] = Nil
     var objectRelationships: List[Displacement] = Nil
     
     val obstacle1 = Displacement(new Measurement(0,0.000001),new Measurement(-2,0.000001))
     val obstacle2 = Displacement(new Measurement(1,0.000001),new Measurement(-1,0.000001))
     val obstacle3 = Displacement(new Measurement(0,0.000001),new Measurement(6,0.000001))
     val obstacle4 = Displacement(new Measurement(-3,0.000001),new Measurement(6,0.000001))
     val obstacle5 = Displacement(new Measurement(-6,0.000001),new Measurement(2,0.000001))
     
     var polarCoord: List[(Measurement,Measurement,Int)] = Nil//theta,r
     polarCoord = (Measurement.atan2(obstacle5.y,obstacle5.x),sqrt(obstacle5.x*obstacle5.x+obstacle5.y*obstacle5.y),2) :: polarCoord
     polarCoord = (Measurement.atan2(obstacle4.y,obstacle4.x),sqrt(obstacle4.x*obstacle4.x+obstacle4.y*obstacle4.y),1) :: polarCoord
     polarCoord = (Measurement.atan2(obstacle3.y,obstacle3.x),sqrt(obstacle3.x*obstacle3.x+obstacle3.y*obstacle3.y),1) :: polarCoord
     polarCoord = (Measurement.atan2(obstacle2.y,obstacle2.x),sqrt(obstacle2.x*obstacle2.x+obstacle2.y*obstacle2.y),1) :: polarCoord
     polarCoord = (Measurement.atan2(obstacle1.y,obstacle1.x),sqrt(obstacle1.x*obstacle1.x+obstacle1.y*obstacle1.y),1) :: polarCoord
     
     println(obstacle1 + " " + Measurement.atan2(obstacle1.y,obstacle1.x))
     println(obstacle2 + " " + Measurement.atan2(obstacle2.y,obstacle2.x))
     println(obstacle3 + " " + Measurement.atan2(obstacle3.y,obstacle3.x))
     println(obstacle4 + " " + Measurement.atan2(obstacle4.y,obstacle4.x))
     println(obstacle5 + " " + Measurement.atan2(obstacle5.y,obstacle5.x))
     
     for(polarReading <- polarCoord)
     {
      println(polarReading +" -> "+ PolarToCartesian(polarReading._2,polarReading._1)) 
      sensorReadings = ObjectReading(polarReading._1, polarReading._2, polarReading._3) :: sensorReadings
     }
     
     objectRelationships = Displacement(1,1) :: Displacement (0,8) :: Displacement(-3,8) :: Nil
     
     identifiedObjects = IdentifiedObject(1,2,Displacement(new Measurement(1,0.000001),new Measurement(1,0.000001))) :: identifiedObjects
     identifiedObjects = IdentifiedObject(1,3,Displacement(new Measurement(0,0.000001),new Measurement(8,0.000001))) :: identifiedObjects
     identifiedObjects = IdentifiedObject(1,4,Displacement(new Measurement(-3,0.000001),new Measurement(8,0.000001))) :: identifiedObjects
     identifiedObjects = IdentifiedObject(2,3,Displacement(new Measurement(-1,0.000001),new Measurement(7,0.000001))) :: identifiedObjects
     identifiedObjects = IdentifiedObject(2,4,Displacement(new Measurement(-4,0.000001),new Measurement(7,0.000001))) :: identifiedObjects
     identifiedObjects = IdentifiedObject(3,4,Displacement(new Measurement(-3,0.000001),new Measurement(0,0.000001))) :: identifiedObjects
     
     identifiedObjects = IdentifiedObject(5,4,Displacement(3,4)) :: identifiedObjects
     identifiedObjects = IdentifiedObject(5,1,Displacement(6,-4)) :: identifiedObjects
     
     cartesianReadings = InternalIdentifiedObject(1,1,obstacle1) :: cartesianReadings
     cartesianReadings = InternalIdentifiedObject(2,1,obstacle2) :: cartesianReadings
     cartesianReadings = InternalIdentifiedObject(3,1,obstacle3) :: cartesianReadings
     cartesianReadings = InternalIdentifiedObject(4,1,obstacle4) :: cartesianReadings
     
     cartesianReadings = InternalIdentifiedObject(5,2,obstacle5) :: cartesianReadings
     
     var mainActor: Actor = actor {
          collectiveMap ! PickName(1,1)
          collectiveMap ! PickName(2,1)
          collectiveMap ! PickName(3,1)
          collectiveMap ! PickName(4,1)
          collectiveMap ! PickName(5,2)
          
          Thread.sleep(2000)//msec
          
          collectiveMap ! Add(updateTime,identifiedObjects,cartesianReadings,sensorRange)
          
          Thread.sleep(2000)//msec
          
          import scala.collection.mutable.Map
          //println(collectiveMap)
          
          /*
          MatchRelationships(identifier1Type: Int,identifier2Type: Int,
	relationshipsDisplacements: List[Displacement],entries: List[Relationship],
	cartesianObjectReadings: List[InternalIdentifiedObject], sensorRange: Double)
                                             
          */
          sensorProcessor ! SensorReadings(sensorRange, sensorReadings)
          /*
          var entries: List[Relationship] = sensorProcessing(cartesianReadings, sensorReadings)
          collectiveMap !? MatchRelationships(1,1,objectRelationships,entries,cartesianReadings,sensorRange) match 
		  {
		     case PossibleMatches(lastUpdate,matches,entries,
                                   cartesianObjectReadings, sensorRange) =>
             {
		       println("Matches: "+matches)
		     }
             case catchall => println("failed: "+ catchall)
          }*/
          //goalFinder ! FindGoal(2,2)
          Thread.sleep(20000)//msec
          collectiveMap ! "Exit"
          goalFinder ! "Exit"
          sensorProcessor ! "Exit"
          agent.exit
          exit
          }
     println("Bye")
   }
}
