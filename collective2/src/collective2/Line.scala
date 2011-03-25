package collective2

import scala.swing._
import java.awt.geom._;
import java.awt.BasicStroke
import java.awt.Stroke
import java.awt.Color
import java.awt.Dimension

class Line(startX: Double, endX: Double, startY: Double, endY: Double) extends Frame{
	val line1 :Line2D = new Line2D.Double(startX, endX, startY, endY);
	val drawingStroke: Stroke = new BasicStroke(2);
	foreground = Color.green/*
	override def paint(graph: Graphics2D){
		println("Printing")
		//graph.setStroke(drawingStroke)
		//graph.setPaint(Color.green)
		graph.draw(line1)
	}*/
}