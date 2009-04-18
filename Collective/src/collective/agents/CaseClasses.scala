package collective.agents

import scala.actors._
import Actor._
import UncertaintyMath.Measurement

sealed abstract class DataClasses
case class MoveCommand(sender: Agent, x: Int, y: Int ) extends DataClasses
case class Displacement(x: Measurement, y: Measurement) extends DataClasses
case class Move(agent: Actor, x: Measurement, y: Measurement) extends DataClasses
case class ObjectReading(angle: Measurement, distance: Measurement) extends DataClasses
case class TopologicalEntry(obstacle1Type: Int,obstacle2Type: Int,
                            deltaX: Measurement, deltaY: Measurement) extends DataClasses
case class IdentifiedObject(identifier1: Int, identifier2: Int, 
                            obstacle1Type: Int,obstacle2Type: Int,
                            deltaX: Measurement, deltaY: Measurement) extends DataClasses
case class UpdateSensor(sender: Agent, range: Int, sensorDeltaAngle: Int, SensorDeltaRange: Int) extends DataClasses

case class Coordinate(x: Int, y: Int) extends DataClasses
case class Obstacle(obstacleType: Int, x: Int, y: Int) extends DataClasses

