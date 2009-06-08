
import scala.swing._

object testGUI extends SimpleGUIApplication{
    val width = 600
    val height =400
    val environmentX = 10
    val environmentY = 10

    def top = new MainFrame{
        title = "Scala Collective"
        contents = new ScalaGUI(environmentX,environmentY,width-20,height-50)
        size = (width,height)
    }
}
