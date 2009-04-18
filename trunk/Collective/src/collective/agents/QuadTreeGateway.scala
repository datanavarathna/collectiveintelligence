package collective.agents
import collective.agents.CaseClasses
import collective.agents.QuadTree
class QuadTreeGateway {
  private val qT = QuadTree
  def add(o: Obstacle): Boolean =
    o2 = ObstacleJava;
    o2.x = o.x;
    o2.y = o.y;
    o2.type = o.Obstacletype;
    qT.add(o2);
    return true
  
   def add(type: int; x: int; y: int): Boolean =
    o2 = ObstacleJava;
    o2.type = type;
    o2.x = x;
    o2.y = y;
    qT.add(o2);
    return true
  
}
