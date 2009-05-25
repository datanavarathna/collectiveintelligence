
import scala.swing._

object testGUI extends SimpleGUIApplication{
    val width = 1920
    val height =1000
    val environmentX = 50
    val environmentY = 50

    def top = new MainFrame{
        title = "Scala Collective"
        contents = new ScalaGUI(environmentX,environmentY,width,height)
        size = (width,height)
    }
}
