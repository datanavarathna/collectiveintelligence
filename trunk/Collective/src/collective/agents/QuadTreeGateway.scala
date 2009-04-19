package collective.agents
import collective.agents.CaseClasses
import collective.agents.QuadTree
class QuadTreeGateway {
  private val qT = QuadTree
  def add(o: Obstacle): Boolean =
    o2 = ObstacleJava;
    o2.x = o.x;
    o2.y = o.y;
    o2.Otype = o.Obstacletype;
    qT.add(o2);
    true
  
   def add(type: Int; x: Int; y: Int): Boolean =
    o2 = ObstacleJava;
    o2.Otype = type;
    o2.x = x;
    o2.y = y;
    qT.add(o2);
    true
  
   def remove(o: Obstacle): Boolean =
     o2 = ObstacleJava;
     o2.x = o.x;
     o2.y = o.y;
     o2.Otype = o.ObstacleType;
     outp = qT.remove(o2);
     outp
   
   def remove(type: Int; x: Int; y: Int): Boolean =
    o2 = ObstacleJava;
    o2.Otype = type;
    o2.x = x;
    o2.y = y;
    o2.Otype = o.ObstacleType;
    outp = qT.remove(o2);
    outp
    
   def contains(o: Obstacle): Boolean =
     o2 = ObstacleJava;
     o2.x = o.x;
     o2.y = o.y;
     o2.Otype = o.ObstacleType;
     outp = qT.find(o2);
     outp
     
   def contains(type: Int; x: Int; y: Int): Boolean =
     o2 = ObstacleJava;
     o2.Otype = type;
     o2.x = x;
     o2.y = y;
     o2.Otype = o.ObstacleType;
     outp = qT.remove(o2);
     outp
    
   def round(radius: Int; x: Int; y: Int): List(Of Obstacle) =
     input = qT.returnAll();
     output = List(Of Obstacle);
     for(i = 0; i < input.length; i++){
       obst = (ObstacleJava)input[i];
       if(((x-obst.x)^2+(y-obst.y)^2)^(0.5)<=radius){
         obst2 = Obstacle;
         obst2.x = obst.x;
         obst2.y = obst.y;
         obst2.ObstacleType = obst.OType;
         output.add(obst2)
       }
     }
     output
}