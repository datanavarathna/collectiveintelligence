import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

public class mousey implements MouseListener {

	public void mouseClicked(MouseEvent arg0) {
		// TODO Auto-generated method stub
	}

	public void mouseEntered(MouseEvent arg0) {
		System.out.println(arg0.getX());
	}

	public void mouseExited(MouseEvent arg0) {
		System.out.println(arg0.getX());
	}

	public void mousePressed(MouseEvent arg0) {
		// TODO Auto-generated method stub
	}

	public void mouseReleased(MouseEvent arg0) {
		// TODO Auto-generated method stub
	}
}
