import scala.actors._
import Actor._
import java.util.ArrayList
import Measurement._

class RelationshipIdentfier(val map: Actor, val sensorProcessor: Actor, agent: Actor) extends Actor{

	def PolarToCartesian(angle: Measurement, distance: Measurement): Displacement =  {
      Displacement(distance * cos(angle), distance * sin(angle))
    }
    
   	def topologicalGenerator(readingA: ObjectReading,readingB: ObjectReading): TopologicalEntry = {
        val aVector: Displacement = PolarToCartesian(readingA.angle,readingA.distance)
        val bVector: Displacement = PolarToCartesian(readingB.angle,readingB.distance)
        val baVector: Displacement = aVector - bVector
        new TopologicalEntry(readingB.obstacleType, readingA.obstacleType,baVector.x, baVector.y)/*,
         List(	 new ObjectReadingCartesian(readingA.obstacleType,aVector),
        		 new ObjectReadingCartesian(readingB.obstacleType,bVector))
         )*/
    }
  
    private def convert(sensorReadings: List[ObjectReading]): List[TopologicalEntry] =
    {
        //var cartesianObjectReadings: List[ObjectReadingCartesian] = Nil
        var entries: List[TopologicalEntry] = Nil
        var readings: List[ObjectReading]  = sensorReadings.asInstanceOf[List[ObjectReading]]
        while(!readings.isEmpty)
        {
        	val readingA = readings.head
            readings = readings.tail
            for(readingB <- readings){
            	val entry = topologicalGenerator(readingA,readingB)
            	entries = entry :: entries
            	//cartesianObjectReadings = cartesianReading ::: cartesianReading
            }
        }//end while
        entries         
    }
    
    private def searchForMatches(topologicalEntries: List[TopologicalEntry], cartesianObjectReadings: List[InternalIdentifiedObject],
                                 relationEntries: List[Relationship], sensorRange: Double)
    {
		println("Search for matchs")
		println("topologicalEntries: " + topologicalEntries)
		println("relationEntries: " + relationEntries)
        import scala.collection.mutable.Map
        import scala.collection.mutable.HashSet
        var type1 = Map.empty[Int,Map[Int,HashSet[Displacement]]]//type1->type2->Displacement
        for(entry <- topologicalEntries)
        {
        	var type2 = type1.getOrElse(entry.obstacle1Type,Map.empty[Int,HashSet[Displacement]])
            var relationships = type2.getOrElse(entry.obstacle2Type,new HashSet[Displacement])
            relationships += Displacement(entry.deltaX,entry.deltaY)
            type2.put(entry.obstacle2Type,relationships)
            type1.put(entry.obstacle1Type,type2)
        }
        println(type1)
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
                            	println("Calling MatchRelationships for " + 
                            			identifier1Type + ", " + identifier2Type + ", " + relationships)
                            	map ! MatchRelationships(identifier1Type,identifier2Type,
									relationships.toList, relationEntries,cartesianObjectReadings, sensorRange)
                            }//end Some
                            case None => {println("Error: No relationships found")}//Should never get to
                        }//end match type2
                     }//end while iterating through type2
                }//end Some type 2
                case None => {println("Error: No type2 found")}//Should never get to
            }//end match type 2
        }//end while type 1 iterator
    }//end search for matches
    
	def mapIdentifiedObjects(	lastUpdate: Long, identifiedObjects: List[IdentifiedObject], sensorEntries: List[Relationship],
                          		cartesianObjectReadings: List[InternalIdentifiedObject], sensorRange: Double) = 
    {
                println("Received map identified objects")
                if(!identifiedObjects.isEmpty)
                {
                	println("There are possible matches to sensor readings")
                    import scala.collection.mutable.Map
                    import scala.collection.mutable.HashSet
                    import scala.collection.mutable.HashMap
                    var possibleMappings = Map.empty[Int,HashSet[Int]]//tempIdentifier->mapIdentifier
                
                    var cartesianReadingsMap = new HashMap[Int,Displacement]
                    for(reading <- cartesianObjectReadings)
                    {
                    	cartesianReadingsMap += (reading.name -> reading.vectorFromAgent)
                    }
                    
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
                    println("possibleMappings: " + possibleMappings)
                    
                    var newIdentifiedObjects: List[IdentifiedObject] = Nil
                    var possibleMappingsIterator = possibleMappings.keys
                    
                    //filter out results that don't fit with sensor readings
                    
                    while(possibleMappingsIterator.hasNext)
                    {
                    	val tempIdentifier = possibleMappingsIterator.next
                        possibleMappings.get(tempIdentifier) match
                        {
                          case Some(mappings) => 
                          {
                            for(mapping <- mappings)
                            {  
                            	(map !? GetRelationsForIdentifier(mapping)) match {
                            	  	case relationsForIdentifier : Map[_,_] =>
                            	  	{
                            	  		val identifierRelations = relationsForIdentifier.asInstanceOf[Map[Displacement,Int]]
                            	  		val relationsIterator = identifierRelations.keys
                            	  		val sensorsMatchMap: Boolean = true
                            	  		while(relationsIterator.hasNext && mappings.contains(mapping))
                            	  		{
                            	  			val relationVector = relationsIterator.next
                            	  			cartesianReadingsMap.get(tempIdentifier) match
                            	  			{
                            	  			  case Some(sensorVector) =>
                            	  				{
                            	  					val mapX = (relationVector.x + sensorVector.x)
                            	  					val mapY = (relationVector.y + sensorVector.y)
                            	  					if(mapX*mapX + mapY*mapY <= sensorRange*sensorRange)
                            	  					{
                            	  					  //should be visible
                            	  					  var detectedVisibleRelation: Boolean = false
                            	  					  for(cartesianReading <- cartesianObjectReadings)
                            	  					  {
                            	  						  val visibleObjectVector = cartesianReading.vectorFromAgent
                            	  						  if(mapX == visibleObjectVector.x && mapY == visibleObjectVector.y)
                            	  							  detectedVisibleRelation = true
                            	  					  }
                            	  					  if(!detectedVisibleRelation)
                            	  					  {
                            	  					    //failed to detect a relation that would be visible if the possible match was correct
                            	  					    mappings.removeEntry(mapping)
                            	  					  }
                            	  					}
                            	  				}//end case sensorVector
                            	  				case None => println("Failed to find a sensorReading for " + tempIdentifier)
                            	  			}//end identifier cartesianSensorReading match
                            	  			
                            	  		}//end while iterating through the relationships of a possible match
                            	  		
                            	  	}//end case for found the relationship for the possible identifier
                            	  	case catchall => println("Received reply: " + catchall)
                            	}//end reply match
                            	
                            }//end for loop for possible matchings
                            
                            if(mappings.size == 1)
                            {
                            	val collectiveMapObjectID = mappings.toList.head
                                //rename object and delete temporary identifier
                                var updatedSensorEntries: List[Relationship] = Nil
                                for(entry <- sensorEntries)
                                {
                                  if(entry.identifier1Temp == tempIdentifier)
                                  {
                                    updatedSensorEntries = new Relationship(collectiveMapObjectID, entry.identifier2Temp,
                                                                            entry.vector) :: updatedSensorEntries
                                  }
                                  else if(entry.identifier2Temp == tempIdentifier)
                                  {
                                    updatedSensorEntries = new Relationship(entry.identifier1Temp, collectiveMapObjectID,
                                                                            entry.vector) :: updatedSensorEntries
                                  }
                                  else
                                    updatedSensorEntries = entry :: updatedSensorEntries
                                }//end converting sensor entries
                                map ! RemoveName(tempIdentifier)//remove tempIdentifier from CollectiveMap
                                sendNewObjectsToAgent(lastUpdate,cartesianObjectReadings,updatedSensorEntries,sensorRange)
                            }//end if mapping is unique
                            else
                            {
                              println("Multiple possible matches remain for sensorEntries: " + sensorEntries)
                              println("Adding sensorEntries to CollectiveMap as is")
                              sendNewObjectsToAgent(lastUpdate,cartesianObjectReadings,sensorEntries,sensorRange)
                            }
                            //possibleMappings.put(tempIdentifier, mappings)//updates possibleMappings to the filtered mappings
                          }//end case Some(mapping)
                          case None => println("Error: couldn't find result of key")//should never get to
                        }//end match for getting possible mappings
                    }//end while loop iterating over all possible mappings
                    
                    
                }//end if not empty
            	else
                {
					println("No possible matches to sensor readings exist, adding to CollectiveMap")
            		sendNewObjectsToAgent(lastUpdate,cartesianObjectReadings,sensorEntries,sensorRange)
                }
                
    }
    
    def sendNewObjectsToAgent(lastUpdate: Long,cartesianObjectReadings: List[InternalIdentifiedObject], sensorEntries: List[Relationship], sensorRange: Double)
    {
    	var newIdentifiedObjects: List[IdentifiedObject] = Nil
      	for(entry <- sensorEntries)
        {
        	newIdentifiedObjects = new IdentifiedObject(entry.identifier1Temp,entry.identifier2Temp, entry.vector) :: newIdentifiedObjects
        }
        agent ! NewIdentifiedObjects(lastUpdate,newIdentifiedObjects,cartesianObjectReadings, sensorRange)
    }
	
    def act()
	{
		println("RelationshipIdentfier Running")
		sensorProcessor ! "Test"
		loop
		{
			react
			{
              
			  case SensorRelations(sensorReadings,cartesianReadings,entries,sensorRange) =>{
            	  println("Received ObjectReadings from sensorProcessor")
            	  val topEntries = convert(sensorReadings)
                  searchForMatches( topEntries, cartesianReadings, entries, sensorRange )
              }//end case
              
              case  RecheckRelationships(reconstructedTopEntries,entries,cartesianReadings,sensorRange) => {
            	  searchForMatches(reconstructedTopEntries,cartesianReadings,entries,sensorRange)
              }
     
              case PossibleMatches(lastUpdate,matches,entries,
                                   cartesianObjectReadings, sensorRange) =>
              {
            	  println("Received possible matches from CollectiveMap")
                  var identifiedObjects : List[IdentifiedObject] = Nil
                  for(identifiedStored <- matches)
                  {
                      identifiedObjects = identifiedStored.idObject :: identifiedObjects
                  }
            	  sensorProcessor ! MapIdentifiedObjects(lastUpdate,identifiedObjects,
            			  					cartesianObjectReadings, sensorRange)

				  if(identifiedObjects.isEmpty)
				  {
                	 println("IdentifiedObjects from possible matches is empty")
				  }
				  //println("Sending: " + MapIdentifiedObjects(lastUpdate,identifiedObjects) )
				  mapIdentifiedObjects(	lastUpdate,identifiedObjects, entries,
						  				cartesianObjectReadings, sensorRange)
				  
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