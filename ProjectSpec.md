# Outline of Project Vision #

DHT – Distributed Hash Table

  * An agent will identify an object by its properties and generate an obstacleType signature of type integer
  * When an agent sees 2 objects, it will measure the distance from the robot to each object and the angle between each object and calculate the distance between the objects using the Law of Cosines.
  * All calculations will be performed with propagation of uncertainty, so that matching is correct despite noise
  * Each agent calculates its relation to objects on the map
    * If an agent wants to know where another agent is, the first agent asks the second agent for its current relations to objects.
  * Calculations can be simplified by assuming each agent contains a compass and assign a global +X and global +Y direction. NOTE: There is no global origin.
  * A vector from d1 to d2 is produced by subtracting the components of d1 from d2.

  * Filter results of matches and only declare a match if only one is left, otherwise identification is not unique. Will need to be changed to handle the same object being assigned different names, so that a relationship between the names can be established.

  * To synchronize shared network data when every agent holds a cache of the entire date, writing access (maybe reads as well) needs to use a token scheme
    * DHT might only need to synch-lock local data storage
  * Data structure must be able to know what changed since last search without actually searching for the added objects.
    * Could be implemented using a linked-list with a location pointer
  * If we use match, the contains function will not work for Measurement because of the inability to match the hashCode to the equals.
  * Data Accesses
Range Search:
[(obstacle1Type, obstacle2Type),VectorFrom1to2(xMeasurement, yMeasurement)]
=>	(identifier1,identifier2)
Must compare VectorFrom1to2 using equals command
> Single Result Search
  * can use a hashSearch
(identifier1,identifier2) =>
> > [(obstacle1Type, obstacle2Type),VectorFrom1to2(xMeasurement, yMeasurement)]


Agent
  * On initialization:
    * Create helper sensor actors
	Touch
	Distance
	Timer driven or an update message could be sent upon moving
    * Add to map function
  * Relations
[(obstacle1Type, obstacle2Type),VectorFrom1to2(xMeasurement, yMeasurement)]
 (identifier1,identifier2)
  * Identifier assigned by checking if the requested identifier is taken and assigning if available
  * Reduces the number of calculations after an object has been uniquely identified


  * Grid drawn based on values from columnTextfield and rowTextfield
  * On EditText for either field, removes old grid from panel contents and adds a newly generated grid
  * Clicking on a default colored block sets color to obstacle color and adds obstacle to obstacle collection. Additional clicking cycles though object types, removing the old object from collection and adding the new collection
  * Clicking on a grid block when obstacle color and the last obstacle type, sets color to agent color, removes obstacle from obstacle collection, and adds an agent to agent collection
  * Clicking on an agent color block sets the block to the default color and removes agent from agent list
  * Make grid squares actors that can receive messages about where an agent is and respond to the message
  * Initializer will initialize agent locations (agents don’t know the absolute locations) from grid coordinates
```
case class Displacement(x: Measurement, y: Measurement)
case class Move(agent: Actor, x: Measurement, y: Measurement)
case class ObjectReading(angle: Measurement, distance: Measurement)
case class topologicalEntries
case class identifiedObjects

object Environment(minx,minY,maxX,maxY)
	def move(x: Int, y: Int) }}}
   * stays within environment limits
   * updates global location of agent
   * tells grid square in old location that agent isn’t there
   * tells grid square in new location that agent is there
   * sends agent message of Displacement 
   * Receives calls to sensors and sends messages to appropriate agents

agent(environment: Actor)
	def move(x: Int, y: Int):Unit
            environment ! Move(this,x,y)
        act()
        {
	      loop
              {
                   react
                   {
			case Displacement( x, y) => {
							relativeLocationX += x
							relativeLocationY += y }
			case Seq[ObjectReading(angle,distance)] =>
	                      pass to helper actor that calculates topological references
                              and sends results as a message to parent actors
	                case Seq[topologicalEntries] =>
	                      send to helper actor that identifies the objects, naming if
                              necessary, messages to parent identify objects
	                case Seq[identifiedObjects] =>
                              addToMapMethod[Seq[identifiedObjects]]
		   }
	       }
         }
```
  * actor maintains a local map with an origin at starting position to assist in location tracking