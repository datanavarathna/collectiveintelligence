package tests

import scala.swing._
import java.awt.geom._;
import java.awt.BasicStroke
import java.awt.Stroke
import java.awt.Color
import java.awt.Dimension
import collective2._

object testGUI extends SimpleSwingApplication{
    val width = 600
    val height = 400
    val environmentX = 10
    val environmentY = 10

    def top = new MainFrame{
        title = "Scala Collective"
        contents = {
        	//new Line(10,300,10,300)
        	new ScalaGUI(environmentX,environmentY,width-20,height-50)
        	
        }
        preferredSize = new Dimension(width,height)
        
    }
}
