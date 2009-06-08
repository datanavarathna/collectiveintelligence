import scala.actors._
import Actor._
import Measurement._
import scala.util.Random

//case class ObjectReading(angle: Measurement, distance: Measurement, obstacleType:Int)
//import CaseClasses._
/*
case class Relationship(identifier1Temp: Int, identifier2Temp: Int, vector: Displacement )
case class InternalIdentifiedObject(name: Int, vectorFromAgent: Displacement)
case class IdentifiedObjects(lastUpdate: Long, identifiedObjects: List[IdentifiedObject])
*/
class SensorObjectReadingsProcessor(val map: Actor,val agent: Actor) extends Actor
{
	var sensorEntries: List[Relationship] = Nil
	//var sensorObjectNames: List[Int] = Nil
	private val randomGenerator: Random = new Random
	private val relationshipIdentifier: Actor = new RelationshipIdentfier(map,this, agent)
 	relationshipIdentifier.start
 
	def PolarToCartesian(angle: Measurement, distance: Measurement): Displacement =  {
      Displacement(distance * cos(angle), distance * sin(angle))
    }
    
    def relationGenerator(readingA: InternalIdentifiedObject, readingB: InternalIdentifiedObject): Relationship = {
        val abVector: Displacement = readingB.vectorFromAgent - readingA.vectorFromAgent
        new Relationship(readingA.name, readingB.name, abVector)
    }

    def nameObstacles(obstacleType: Int): Int =
    {
      var objectName: Int = randomGenerator.nextInt
      while( (map !? PickName(objectName,obstacleType)) == "Failed to add name"){
        objectName = randomGenerator.nextInt
      }
      //sensorObjectNames = objectName :: sensorObjectNames
      println("objectName = " + objectName)
      objectName
    }
    
    def internallyIdentifyObjects(readings: List[ObjectReading]): List[InternalIdentifiedObject] =
    {
        var results: List[InternalIdentifiedObject] = Nil
        for(reading <- readings)
        {
            results = new InternalIdentifiedObject( nameObstacles(reading.obstacleType),reading.obstacleType,
                                                PolarToCartesian(reading.angle,reading.distance) ) :: results
        }
        results
    }
    
    def act()
	{
		println("SensorObjectReadingsProcessor Running")
		loop
		{
			react
			{
			  case SensorReadings(sensorRange, sensorReadings) =>{
            	  	  println("SensorProcessor Received ObjectReadings from Agent")
                
            	  	  
            	  	  //sensorObjectNames = Nil
                      var entries: List[Relationship] = Nil
                      val cartesianReadings = internallyIdentifyObjects(sensorReadings.asInstanceOf[List[ObjectReading]])
                      var readings: List[InternalIdentifiedObject]  = cartesianReadings
                      while(!readings.isEmpty){
                          val readingA = readings.head
                          readings = readings.tail
                          for(readingB <- readings){
                              entries = relationGenerator(readingA,readingB) :: entries
                          }
                      }//end while
                      println("Relationships from readings: " + entries)
                      println("cartesianReadings: " + cartesianReadings)
                      println("readings: " + readings)
                      sensorEntries = entries //send List[Relationship] to agent
					  relationshipIdentifier ! SensorRelations(sensorReadings.asInstanceOf[List[ObjectReading]],
                                                cartesianReadings,entries,sensorRange)
					  
              }//end case

              //MapIdentifiedObjects(lastUpdate: Long, identifiedObjects: List[IdentifiedObject])
              case MapIdentifiedObjects(lastUpdate,identifiedObjects,cartesianReadings,sensorRange) => {
                println("Received map identified objects")
                if(!identifiedObjects.isEmpty)
                {
                    import scala.collection.mutable.Map
                    import scala.collection.mutable.HashSet
                    var possibleMappings = Map.empty[Int,HashSet[Int]]//tempIdentifier->mapIdentifier
                
                    for(entry <- sensorEntries)
                    {
                    	for(identifiedObject <- identifiedObjects)
                    	{
                    		if(entry.vector == identifiedObject.vector)
                    		{
                    			var tempHashSet = possibleMappings.getOrElse(entry.identifier1Temp, new HashSet[Int]);
								tempHashSet += identifiedObject.identifier1
								possibleMappings.put(entry.identifier1Temp,tempHashSet)
								tempHashSet = possibleMappings.getOrElse(entry.identifier2Temp, new HashSet[Int]);
                    			tempHashSet += identifiedObject.identifier2
								possibleMappings.put(entry.identifier2Temp,tempHashSet)
                    		}
                    	}//end objects for
                    }//end entries for
                    
                    //filter out results that don't fit with sensor readings
                    
                }//end if not empty
            	else
                {
					println("No possible matches to sensor readings exist")
            		var newIdentifiedObjects: List[IdentifiedObject] = Nil
            		for(entry <- sensorEntries)
            		{
            			newIdentifiedObjects = new IdentifiedObject(entry.identifier1Temp,entry.identifier2Temp, entry.vector) :: newIdentifiedObjects
            		}
            		agent ! NewIdentifiedObjects(lastUpdate,newIdentifiedObjects,cartesianReadings,sensorRange)
                }
                
              }
              
              case RecheckObjects(identifiedObjects,cartesianObjectReadings, sensorRange) => {
				  println("Received recheck from agent")
            	  var reconstructedTopEntries : List[TopologicalEntry] = Nil
            	  for(identifiedObject <- identifiedObjects)
                  {
            		  val relationVector = identifiedObject.vector
            		  map !? GetIdentifierType(identifiedObject.identifier1) match {
            		    case IdentifierType(identifier1,objectType1) => {
            		      map !? GetIdentifierType(identifiedObject.identifier2) match {
            		        case IdentifierType(identifier2,objectType2) => {
            		          reconstructedTopEntries = new	TopologicalEntry(objectType1,objectType2,
            		        		  relationVector.x, relationVector.y) :: reconstructedTopEntries
            		        }//end case
            		        case catchall => {println(catchall)}//should never get to
            		      }//match identifier2
            		    }//end case
            		    case catchall => {println(catchall)}//should never get to
            		  }//match identifier1  
                  }//end for
				  var reconstructedEntries: List[Relationship] = Nil
				  for(identifiedObject <- identifiedObjects)
            		{
            			reconstructedEntries = new Relationship(identifiedObject.identifier1,
							identifiedObject.identifier2, identifiedObject.vector) :: reconstructedEntries
            		}
                  relationshipIdentifier ! RecheckRelationships(reconstructedTopEntries,reconstructedEntries,
                  												cartesianObjectReadings, sensorRange)
				  
              }
              case "Exit" => {
                 println("SensorObjectReadingsProcessor Exiting")
                 this.exit
              }
			  case catchall => {println("sensorProcessor caught " + catchall) }
			}//end react
		}//end loop
	}//end act
}
