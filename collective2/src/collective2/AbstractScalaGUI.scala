package collective2

import scala.swing._

abstract class AbstractScalaGUI extends FlowPanel{
	def getWorldButton(index: Int): RegionButton
	def getEnvironmentX: Int
}