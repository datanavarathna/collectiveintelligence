package collective.agents

import agents.DataClasses

class QuadTree {
  def contains(x: Int,y: Int):Boolean
  def add(x: Int,y: Int):Boolean //returns true if successful
  def remove(x: Int,y: Int):Boolean //returns true if successful
  def range(minX: Int, minY: Int, maxX: Int, maxY: Int):List[Obstacle]
}
