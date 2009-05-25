import scala.actors._
import Actor._
import java.util.ArrayList
import Measurement._

/*//The following commented code is for reference only
case class TopologicalEntry(obstacle1Type: Int,obstacle2Type: Int,
                            deltaX: Measurement, deltaY: Measurement)
case class IdentifiedObject(identifier1: Int, identifier2: Int,
                            vector: Displacement)
case class ObjectReading(angle: Measurement, distance: Measurement, obstacleType:Int)
case class Displacement(x: Measurement, y: Measurement)
*/
case class PickName(identifier: Int, obstacleType: Int)
case class MapSize()
case class Size(size: Int)
case class GetIdentifierType(identifier: Int)
case class Add(identifiedObject: IdentifiedObject)
case class Contains(identifiers: Identifiers)
case class Matches(entries: TopologicalEntry *)
case class IdentifierType(identifier: Int,objectType: Int)
case class noType(identifier: Int)

case class TargetDisplacement(x: Measurement, y: Measurement)
case class TimeSinceLastUpdate(time: Long)
case class Identifiers(a: Int, b: Int){
    def inverse: Identifiers = Identifiers(b,a)
}
case class MatchRelationships(identifier1: Int,identifier2: Int,relationshipsDisplacements: List[Displacement])
case class IdentifiedStored(idObject:IdentifiedObject) extends Ordered[IdentifiedStored]{
    private val dispX: Measurement = idObject.vector.x
    private val dispY: Measurement = idObject.vector.y
    val dSquared = dispX*dispX + dispY*dispY

    def compare(that : IdentifiedStored) : Int ={
        val thisX = this.idObject.vector.x
        val thisY = this.idObject.vector.y
        val thatX = that.idObject.vector.x
        val thatY = that.idObject.vector.y
        if( (thisX == thatX) && (thisY == thatY) )
            return 0
        else if(this.dSquared < that.dSquared)
            return -1
        else
            return 1
    }

}
case class RelationshipStored(vector: Displacement) extends Ordered[RelationshipStored]
{
    private val x = vector.x
    private val y = vector.y
    val dSquared = x*x + y*y
    
    override def toString = "RelationshipStored("+ vector + "," + dSquared + ") "
    
    def compare(that : RelationshipStored) : Int ={
        val thatX = that.vector.x
        val thatY = that.vector.y
        if( (x == thatX) && (y == thatY) )
            return 0
        else if(this.dSquared < that.dSquared)
            return -1
        else
            return 1
    }
}
class TopologicalElementGenerator(val map: Actor) extends Actor{
	
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

    def act()
	{
		println("TopologicalElementGenerator Running")
		loop
		{
			react
			{
              case sensorReadings @ List(ObjectReading,_*) =>{
                      //convert to topological entries and reply(entries)
                      var entries: List[TopologicalEntry] = Nil
                      var readings: List[ObjectReading]  = sensorReadings.asInstanceOf[List[ObjectReading]]
                      while(!readings.isEmpty){
                          val readingA = readings.head
                          readings = readings.tail
                          for(readingB <- readings){
                              entries = topologicalGenerator(readingA,readingB) :: entries
                          }
                      }//end while
                      reply(entries)
              }//end case

                case "Exit" => {
                 println("TopologicalElementGenerator Exiting")
                 this.exit
              }
			}//end react
		}//end loop
	}//end act
}

class RelationshipIdentfier(val map: Actor) extends Actor{

    def act()
	{
		println("RelationshipIdentfier Running")
		loop
		{
			react
			{
              case topologicalEntries @ List(TopologicalEntry, _*) => {
                 //reply()//convert topological entries to identified objects and send in reply
                 val entries = topologicalEntries.asInstanceOf[List[TopologicalEntry]]
                 import scala.collection.mutable.Map
                 import scala.collection.mutable.HashSet
                 var type1 = Map.empty[Int,Map[Int,HashSet[Displacement]]]
                 for(entry <- entries)
                 {
                     var type2 = type1.getOrElse(entry.obstacle1Type,Map.empty[Int,HashSet[Displacement]])
                     var relationships = type2.getOrElse(entry.obstacle2Type,new HashSet[Displacement])
                     relationships += Displacement(entry.deltaX,entry.deltaY)
                     type2.put(entry.obstacle2Type,relationships)
                     type1.put(entry.obstacle1Type,type2)
                 }
                 var identifier1: Int = 0
                 var identifier2: Int = 0
                 var type1Iterator = type1.keys
                 while(type1Iterator.hasNext)
                 {
                     identifier1 = type1Iterator.next
                     type1.get(identifier1) match
                     {
                         case Some(type2) =>
                         {
                            var type2Iterator = type2.keys
                            while(type2Iterator.hasNext)
                            {
                                identifier2 = type2Iterator.next
                                type2.get(identifier2) match
                                {
                                    case Some(relationships) =>
                                    {
                                        map ! MatchRelationships(identifier1,identifier2,relationships.toList)
                                    }
                                    case None => {}
                                }
                            }
                         }
                         case None => {}
                     }

                 }
              }
              case "Exit" => {
                 println("RelationshipIdentfier Exiting")
                 this.exit
              }
			}//end react
		}//end loop
	}//end act
}

class GoalFinder(val agent: Actor, val map: Actor) extends Actor
{
    def act()
	{
		println("GoalFinder Running")
		loop
		{
			react
			{
              case Goal(goal) => {
                 //find goal
                 val xDisplacementToGoal = 2//temporary
                 val yDisplacementToGoal = 1//temporary
                 var foundGoal: Boolean = true
                 if(foundGoal){
                      //agent ! TargetDisplacement(xDisplacementToGoal,yDisplacementToGoal)
                      agent ! List(Coordinate(xDisplacementToGoal,yDisplacementToGoal))
                 }
                 else
                    agent ! GoalNotFound()
              }
              case "Exit" => {
                 println("Goalfinder Exiting")
                 this.exit
              }
			}//end react
		}//end loop
	}//end act
}

class CollectiveMap extends Actor
{
	private var updateTime = System.currentTimeMillis()
	private var size: Int = 0

    import scala.collection.mutable.Map
    import scala.collection.mutable.HashMap
    import scala.collection.jcl.TreeMap
    import scala.collection.jcl.TreeSet
    private var relationshipsLookup = Map.empty[Identifiers,Displacement]//key,value
    private var identifierType = Map.empty[Int,Int]// (identifier -> type)
    private var obstacle1TypeMap = Map.empty[Int,HashMap[Int,TreeMap[RelationshipStored,Identifiers]]]//key,value obstacleType1->obstacleType2->relationship->Identifiers
    private var identifierRelationshipsGraph = Map.empty[Int,TreeMap[RelationshipStored,Int]]//identifier1->relationships->identifier2

    /*
    private def findRelationships(identifier1: Int, identifier2: Int):TreeSet[IdentifiedStored] = {
        var obstacle2Map = obstacle1Map.getOrElse(identifier1,
            new TreeMap[Int,TreeSet[IdentifiedStored]])
        obstacle2Map.getOrElse(identifier2,
            new TreeSet[IdentifiedStored])
    }*/
    
    override def toString = {
    		var result = "Collective Map \n"
    		result += "  relationshipLookup\n" + relationshipsLookup
    		result += "\n  identifierType\n" + identifierType
    		result += "\n  obstacle1TypeMap\n" + obstacle1TypeMap
    		result += "\n  identifierRelationshipsGraph" + identifierRelationshipsGraph
    		result
    }
    
    def pickName(identifier: Int, obstacleType: Int): Boolean =
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
    
    def getIdentifierType(identifier: Int): Option[Int] = 
    {
    		identifierType.get(identifier)
    }

    def add(relationship: IdentifiedObject): Boolean = 
    {
        updateTime = System.currentTimeMillis()

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
        	identifier1, new TreeMap[RelationshipStored,Int])
        
        var obstacle2TypeMap = obstacle1TypeMap.getOrElse(identifier1Type,
            new HashMap[Int,TreeMap[RelationshipStored,Identifiers]])
        var relationships = obstacle2TypeMap.getOrElse(identifier2Type,
            new TreeMap[RelationshipStored,Identifiers])

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
                if(obstacle2TypeMap.contains(identifier1Type))
                {
                	relationshipsLookup += (identifiers -> relationship.vector)
                    if(contains(identifiers)){
                    	//size += 1
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
/*
    def delete(relationship: IdentifiedObject): Boolean = 
    {
       val identifier1 = relationship.identifier1
        val identifier2 = relationship.identifier2
        val identifiers = Identifiers(identifier1,identifier2)

        if(contains(identifiers))
        {
           var identifierA = identifier1
           var identifierB = identifier2
           var obstacle2TypeMap = new HashMap[Int,TreeMap[RelationshipStored,Identifiers]]
           var relationships = new TreeMap[RelationshipStored,Identifiers]
           obstacle1TypeMap.get(identifier1) match
           {
               case Some(map) =>
               {
                       var obstacle2TypeMap = map
                       obstacle2TypeMap.get(identifier2) match{
                           case Some(idRelationships) => {
                               relationships = idRelationships
                           }
                           case None =>{
                                println("Uneven Data Structure: Data not added to structure evenly")
                           }
                       }//end 2nd level match
               }//end regular Some case
               case None => 
               {
                       identifierA = identifier2
                       identifierB = identifier1
                       obstacle1TypeMap.get(identifier2) match{
                            case Some(map) =>{
                                var obstacle2TypeMap = map
                                obstacle2TypeMap.get(identifier1) match{
                                    case Some(idRelationships) => {
                                        relationships = idRelationships
                                    }
                                    case None =>{
                                        println("Uneven Data Structure: Data not added to structure evenly")
                                    }
                                }//end 2nd level match
                            }//end inverse Some case
                            case None => {
                                println("Uneven Data Structure: Data not added to structure evenly")
                            }
                       }//end inverse match
               }//end regular None casse
           }//end regular match
           
            val addKey = RelationshipStored(relationship.vector)
            val addItem = Identifiers(relationship.identifier1,
                        		  relationship.identifier2)
            val addItemInverse = IdentifiedStored(relationship.inverse)

            relationships -= addKey
            //relationships -= addItemInverse
            if(relationships.contains(addKey) /*&& !relationships.has(addItemInverse)*/)//boolean to check if successful
            {
                obstacle2TypeMap.put(identifierB,relationships)
                if(obstacle2TypeMap.contains(identifierB))
                {
                    obstacle1TypeMap.put(identifierA,obstacle2TypeMap)
                    if(obstacle2TypeMap.contains(identifierA))
                    {
                        relationshipsLookup -= (identifiers)
                        relationshipsLookup -= (identifiers.inverse)
                        if(!contains(identifiers)){
                            return true
                        }
                        else{
                            println("Failed to remove identifiers to relationshipsLookup")
                            return false
                        }//end add to relationshipsLookup check
                    }
                    else{
                        println("Failed to remove element to obstacle1TypeMap")
                        return false
                    }//end add to obstacle1TypeMap check
                }
                else{
                    println("Failed to remove element to obstacle2TypeMap")
                    return false
                }//end add to obstacle2TypeMap check
            }
            else{
                println("Failed to remove element to relationships")
                return false
            }//end add to relationships check
        }
        else
            return true//nothing to delete
    }//end delete
*/
    def contains(identifiers: Identifiers):Boolean = {
        if(relationshipsLookup.contains(identifiers))
            return true
        else{
            val inverseIdentifiers = Identifiers(identifiers.b,identifiers.a)
            return relationshipsLookup.contains(inverseIdentifiers)
        }
    }

    def matches(entries: TopologicalEntry *):List[IdentifiedObject] = {
        import scala.collection.mutable.HashSet
        var matches = new HashSet[Identifiers]

        Nil.asInstanceOf[List[IdentifiedObject]]
    }

    //always check contains before calling
    def getRelationship(relationship: Identifiers):IdentifiedObject = {
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

    def matchRelationships(identifier1: Int,identifier2: Int,relationshipsDisplacements: List[Displacement]): List[Identifiers] =
    {
		var matches = new TreeSet[IdentifiedStored]
        obstacle1TypeMap.get(identifier1) match
        {
            case Some(obstacle2Map) =>
            {
                obstacle2Map.get(identifier2) match 
                {
                  case Some(identifiedStoredTreeSet) =>
                  {
                    
                  }
                  case None => return Nil 
                }
                return Nil
            }
            case None => return Nil
        }

    }

    //if the map is empty, objects need to be assigned random identifiers and added to map

    def act()
	{
		println("CollectiveMap Running")
		loop
		{
			react
			{
			  case PickName(identifier,obstacleType) => {
				  if(pickName(identifier,obstacleType))
				  {
					  println("Added name")
					  reply("Added name")
				  }
				  else
				  {
					  println("Failed to add name")
					  reply("Failed to add name")
				  }
			  }
			  case MapSize => {
				  println("Map size: " + size)
				  reply(Size(size))
			  }
			  case GetIdentifierType(identifier: Int) => {
				  getIdentifierType(identifier) match {
				    case Some(objectType) => reply(IdentifierType(identifier,objectType))
				    case None => reply(noType(identifier))
				  }
			  }
			  case Contains(identifiers: Identifiers) => {}
			  case Matches(entries) => {}
              case Add(identifiedObject) => {
            	  println("Attempting to add object")
                    if(!add(identifiedObject))
                        println("Failed to add object")
                    else
                    {
                    	println("Attempting to add inverse object")
                    	if(!add(identifiedObject.inverse))
                    		println("Failed to add inverse object")
                    	else
                    		size += 1
                    }
              }
/*              case Contains(entries) => {
                   reply(matches(entries))
              } */
              case MatchRelationships(identifier1,identifier2,relationshipsDisplacements) =>
              {
                  reply(matchRelationships(identifier1,identifier2,relationshipsDisplacements))
              }
              case "lastUpdate" =>
                  reply(TimeSinceLastUpdate(System.currentTimeMillis()-updateTime))
              case "Exit" => {
                 println("CollectiveMap Exiting")
                 this.exit
              }//end case Exit
              case catchall => println("catchall: " + catchall) 
			}//end react
		}//end loop
	}//end act
}