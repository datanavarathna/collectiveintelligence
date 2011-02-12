package collective2

import scala.actors._
import Actor._
import java.util.ArrayList
import uncertaintyMath.Measurement
import uncertaintyMath.Measurement._

class CollectiveMap extends Actor
{
	/*
	private var updateTime: Long = System.currentTimeMillis()
	private var size: Int = 0

    import scala.collection.mutable.Map
    import scala.collection.mutable.HashMap
    import scala.collection.mutable.HashSet
    import scala.collection.jcl.TreeMap
    import scala.collection.jcl.TreeSet
    private var relationshipsLookup = Map.empty[Identifiers,Displacement]//key,value
    private var identifierType = Map.empty[Int,Int]// (identifier -> type)
    private var obstacle1TypeMap = Map.empty[Int,HashMap[Int,UncertaintyMap[Identifiers]]]//key,value obstacleType1->obstacleType2->relationship->Identifiers
    private var identifierRelationshipsGraph = Map.empty[Int,UncertaintyMap[Int]]//identifier1->relationships->identifier2
    
    override def toString = {
    		var result = "Collective Map \n"
    		result += "  relationshipLookup\n" + relationshipsLookup
    		result += "\n  identifierType\n" + identifierType
    		result += "\n  obstacle1TypeMap\n" + obstacle1TypeMap
    		result += "\n  identifierRelationshipsGraph" + identifierRelationshipsGraph
    		result
    }
    
    private def getSize(): CollectiveMapSize =
    {
        new CollectiveMapSize(size,updateTime)
    }
    
    private def pickName(identifier: Int, obstacleType: Int): Boolean =
    {
        if(!identifierType.contains(identifier))
        {
        	identifierType += (identifier -> obstacleType)
        	return identifierType.contains(identifier)
        }
        else
        {
          println("Name exists for :" + identifier)
          return false
        }
    }
    
    private def removeName(identifier: Int): Boolean = {
    	identifierType -= (identifier)
    	return !identifierType.contains(identifier)
    }
    
    private def getIdentifierType(identifier: Int): Option[Int] = 
    {
    		identifierType.get(identifier)
    }

    private def add(relationship: IdentifiedObject): Boolean = 
    {

        val identifier1 = relationship.identifier1
        val identifier2 = relationship.identifier2
        val identifiers = Identifiers(identifier1,identifier2)
        
        val identifier1Type: Int = getIdentifierType(identifier1) match 
        {
          case Some(identifierType) => identifierType
          case None =>
          {
        	  println("Error: type not known for " + identifier1)
        	  return false
          }
        }
        val identifier2Type: Int = getIdentifierType(identifier2) match 
        {
          case Some(identifierType) => identifierType
          case None =>
         {
        	  println("Error: type not known for " + identifier2)
        	  return false
          }
        }
        
        val graphRelationshipsStored = identifierRelationshipsGraph.getOrElse(
        	identifier1, new UncertaintyMap[Int])
        
        var obstacle2TypeMap = obstacle1TypeMap.getOrElse(identifier1Type,
            new HashMap[Int,UncertaintyMap[Identifiers]])
        var relationships = obstacle2TypeMap.getOrElse(identifier2Type,
            new UncertaintyMap[Identifiers])

        val addKey = RelationshipStored(relationship.vector)
        val addItem = Identifiers(relationship.identifier1,
                        		  relationship.identifier2)
        
        relationships += (addKey -> addItem)
        graphRelationshipsStored += (addKey -> identifier2)
        if(graphRelationshipsStored.contains(addKey))
        {
            identifierRelationshipsGraph.put(identifier1,graphRelationshipsStored)
            
        }
        else{
          println("Failed to add relation to graph")
          return false
        }
        
        if(relationships.contains(addKey))//boolean to check if successful
        {
            obstacle2TypeMap.put(identifier2Type,relationships)
            if(obstacle2TypeMap.contains(identifier2Type))
            {
                obstacle1TypeMap.put(identifier1Type,obstacle2TypeMap)
                if(obstacle1TypeMap.contains(identifier1Type))
                {
                	relationshipsLookup += (identifiers -> relationship.vector)
                    if(contains(identifiers)){
                    	//size += 1
                    	updateTime = System.currentTimeMillis()
                    	println("Last update on "+updateTime)
                        return true
                    }
                    else{
                        println("Failed to add identifiers to relationshipsLookup")
                        return false
                    }//end add to relationshipsLookup check
                }
                else
                {
                    println("Failed to add element to obstacle1TypeMap")
                    return false
                }//end add to obstacle1TypeMap check
            }
            else
            {
                println("Failed to add element to obstacle2TypeMap")
                return false
            }//end add to obstacle2TypeMap check
        }
        else{
            println("Failed to add element to relationships")
            return false
        }//end add to relationships check
    }//end add

    private def contains(identifiers: Identifiers):Boolean = {
        if(relationshipsLookup.contains(identifiers))
            return true
        else{
            val inverseIdentifiers = Identifiers(identifiers.b,identifiers.a)
            return relationshipsLookup.contains(inverseIdentifiers)
        }
    }

    private def matches(entries: TopologicalEntry *):List[IdentifiedObject] = {
        import scala.collection.mutable.HashSet
        var matches = new HashSet[Identifiers]

        Nil.asInstanceOf[List[IdentifiedObject]]
    }

    //always check contains before calling
    private def getRelationship(relationship: Identifiers):IdentifiedObject = {
    	relationshipsLookup.get(relationship) match {
            case Some(vector) => {
                    IdentifiedObject(relationship.a,relationship.b,vector)
            }
            case None => {
                   relationshipsLookup.get(relationship) match {
                      case Some(vector) => {
                        IdentifiedObject(relationship.a,relationship.b,vector.inverse)
                        }
                        case None => {
                            println("No relationship found")
                            IdentifiedObject(0,0,Displacement(0,0))
                        }
                   }//end 2nd match
            }//end case None
        }
    }

    def matchRelationships(identifier1Type: Int,identifier2Type: Int,relationshipsDisplacements: List[Displacement]): List[IdentifiedStored] =
    {
		println("matchRelationships received: " + identifier1Type +", "+ identifier2Type +", relDisp= "+
			relationshipsDisplacements)
		var matches = new TreeSet[IdentifiedStored]
		println("map size= " + size)
		//var possibleMatches = new HashSet[Identifiers]
        obstacle1TypeMap.get(identifier1Type) match
        {
            case Some(obstacle2Map) =>
            {
                obstacle2Map.get(identifier2Type) match 
                {
                  case Some(identifiedStoredTreeSet) => //UncertaintyMap[RelationsStored,identifiers]
                  {
                    //get all matches and prepend 'matches'
                    var storedRelationships: List[RelationshipStored] = Nil;
                    for(relationshipDisplacement <- relationshipsDisplacements)
                    {
                    	println("getting all equals for " + relationshipDisplacement)
						var identifiers = identifiedStoredTreeSet.getAllEquals(RelationshipStored(relationshipDisplacement))
						println("allEqualsFor " + relationshipDisplacement +": " + identifiers)
						//println("in " + identifiedStoredTreeSet)
                    	/*for(identifier <- identifiers)
                        {
                    		possibleMatches += identifier
                        }*/
                        var numOfPossibleMatches = identifiers.size
                    	if( numOfPossibleMatches == 1)
                    	{
							println("Only one possible match")
                    		var identifier = identifiers.head
                    		relationshipsLookup.get(identifier) match
                    		{
                    		  	case Some(vector) => {
                    			  matches += IdentifiedStored(IdentifiedObject(identifier.a, identifier.b,vector))
                    			}
                    		  	case None => { println("relationshipsLookup does not contain " + identifier)}//should never get to
                    		}//end match
                    		
                        }// end if uniquely identified
                     else if(numOfPossibleMatches > 1)
                        {
                        	println("Multiple possible matches")
							for(identifier <- identifiers)
							{
								relationshipsLookup.get(identifier) match
								{
									case Some(vector) => {
										matches += IdentifiedStored(IdentifiedObject(identifier.a, identifier.b,vector))
									}
									case None => { println("relationshipsLookup does not contain " + identifier)}//should never get to
								}//end match
							}
							
							//see if comparisons with agents sensors & local map of obstacles can uniquely identify
                        }//end not unique
                    		
                    }//end for relationshipsDisplacements
                    //possibleMatchesGraph(possibleMatches.toList)
                    
                  }//end matched identifier
                  case None => 
                  {
                    println("No relations for type " + identifier1Type + " -> type " + identifier2Type)
                    return Nil 
                  }//end case None
                }//end match type 2
				println("matchRelations returning " + matches.toList)
                return matches.toList //currently returns list of already existing relations
            }
            case None =>
            {
              println("No relations for type " + identifier1Type)
              return Nil
            }
        }

    }

    def possibleMatchesGraph(identifiers: List[Identifiers]): Map[Int,UncertaintyMap[Int]] = 
    {
    	var results = Map.empty[Int,UncertaintyMap[Int]]
    	for(identifier <- identifiers)
    	{
    		var relationshipsStored : UncertaintyMap[Int] = results.getOrElse(identifier.a,new UncertaintyMap[Int])
    		relationshipsLookup.get(identifier) match
    		{
    		  case Some(vector)=> 
    		  {
    			  var addKey = RelationshipStored(vector)
    			  relationshipsStored += ( addKey -> identifier.b)
    			  if(relationshipsStored.contains(addKey))
    			  {
    				  results.put(identifier.a,relationshipsStored)
			      }
			      else{
			         println("Failed to add relation to possible matches graph")
			      }
    		  }
    		  case None => { println("relationshipsLookup does not contain " + identifier)}//should never get to
    		}//end match
    	}//end for identifiers
    	return results
    }
    
    //if the map is empty, objects need to be assigned random identifiers and added to map
*/
    def act()
	{
		println("CollectiveMap Running")
		/*
		loop
		{
			react{
				
			}
		}
		*/
	}//end act
	
}