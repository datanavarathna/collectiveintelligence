import scala.actors._
import Actor._
import java.util.ArrayList
import Measurement._
//import CaseClasses._
//case class PossibleMatches(lastUpdate: Long,matches : List[IdentifiedStored])

class RelationshipIdentfier(val map: Actor, val sensorProcessor: Actor, agent: Actor) extends Actor{

	def PolarToCartesian(angle: Measurement, distance: Measurement): Displacement =  {
      Displacement(distance * cos(angle), distance * sin(angle))
    }
    
   	def topologicalGenerator(readingA: ObjectReading,readingB: ObjectReading): TopologicalEntry = {
        val aVector: Displacement = PolarToCartesian(readingA.angle,readingA.distance)
        val bVector: Displacement = PolarToCartesian(readingB.angle,readingB.distance)
        val baVector: Displacement = aVector - bVector
        new TopologicalEntry(readingB.obstacleType, readingA.obstacleType,
                baVector.x, baVector.y
            )
    }
  
    private def convert(sensorReadings: List[ObjectReading]):List[TopologicalEntry] =
    {
        var entries: List[TopologicalEntry] = Nil
        var readings: List[ObjectReading]  = sensorReadings.asInstanceOf[List[ObjectReading]]
        while(!readings.isEmpty)
        {
        	val readingA = readings.head
            readings = readings.tail
            for(readingB <- readings){
            	entries = topologicalGenerator(readingA,readingB) :: entries
            }
        }//end while
        entries              
    }
    
    private def searchForMatches(topologicalEntries: List[TopologicalEntry], relationEntries: List[Relationship])
    {
		
       	val entries = topologicalEntries.asInstanceOf[List[TopologicalEntry]]
        import scala.collection.mutable.Map
        import scala.collection.mutable.HashSet
        var type1 = Map.empty[Int,Map[Int,HashSet[Displacement]]]//type1->type2->Displacement
        for(entry <- entries)
        {
        	var type2 = type1.getOrElse(entry.obstacle1Type,Map.empty[Int,HashSet[Displacement]])
            var relationships = type2.getOrElse(entry.obstacle2Type,new HashSet[Displacement])
            relationships += Displacement(entry.deltaX,entry.deltaY)
            type2.put(entry.obstacle2Type,relationships)
            type1.put(entry.obstacle1Type,type2)
        }
        var identifier1Type: Int = 0
        var identifier2Type: Int = 0
        var type1Iterator = type1.keys
        while(type1Iterator.hasNext)
        {
        	identifier1Type = type1Iterator.next
            type1.get(identifier1Type) match
            {
            	case Some(type2) =>
                {
                	var type2Iterator = type2.keys
                    while(type2Iterator.hasNext)
                    {
                    	identifier2Type = type2Iterator.next
                        type2.get(identifier2Type) match
                        {
                        	case Some(relationships) =>
                            {
                            	map ! MatchRelationships(identifier1Type,identifier2Type,
									relationships.toList, relationEntries)
                            }//end Some
                            case None => {}//Should never get to
                        }//end match type2
                     }//end while iterating through type2
                }//end Some type 2
                case None => {}//Should never get to
            }//end match type 2
        }//end while type 1 iterator
    }//end search for matches
    
	def mapIdentifiedObjects(lastUpdate: Long, identifiedObjects: List[IdentifiedObject], sensorEntries: List[Relationship]) = {
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
            		agent ! NewIdentifiedObjects(lastUpdate,newIdentifiedObjects)
                }
                
              }
	
    def act()
	{
		println("RelationshipIdentfier Running")
		sensorProcessor ! "Test"
		loop
		{
			react
			{
              
			  case SensorRelations(sensorReadings,entries) =>{
            	  	  println("Received ObjectReadings from sensorProcessor")
                
                  searchForMatches( convert(sensorReadings),entries )
              }//end case
              
              case  RecheckRelationships(reconstructedTopEntries,entries) => {
            	  searchForMatches(reconstructedTopEntries,entries)
              }
     
              case PossibleMatches(lastUpdate,matches,entries) =>
              {
            	  println("Received possible matches from CollectiveMap")
                  var identifiedObjects : List[IdentifiedObject] = Nil
                  for(identifiedStored <- matches)
                  {
                      identifiedObjects = identifiedStored.idObject :: identifiedObjects
                  }
            	  sensorProcessor ! MapIdentifiedObjects(lastUpdate,identifiedObjects)

				  if(identifiedObjects.isEmpty)
				  {
                	 println("IdentifiedObjects from possible matches is empty")
				  }
				  //println("Sending: " + MapIdentifiedObjects(lastUpdate,identifiedObjects) )
				  mapIdentifiedObjects(lastUpdate,identifiedObjects, entries)
				  
              }
              
              case "Exit" => {
                 println("RelationshipIdentfier Exiting")
                 this.exit
              }
			  case catchAll => println("RelationIdentifier Catchall: " + catchAll)
			}//end react
		}//end loop
	}//end act
}