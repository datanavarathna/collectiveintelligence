package collective.gui;

import javax.swing.JButton;


public class GUIObject extends JButton {
	private static final long serialVersionUID = 1L;
	int status;
	
	GUIObject( int status ) {
		super("");
		this.status = status;
		this.updateText();
	}
	
	private void updateText() {
		String labelText = "";
		
		if ( this.status == 0 ) {
			labelText = "";
		} else if ( this.status == 1 ) {
			labelText = "Agent Here";
		} else if ( this.status == 2 ) {
			labelText = "Object Here";
		} else {
			labelText = "";
		}
		
		this.setText(labelText);
	}
	
	public int getStatus() { return this.status; }
	
	public void setStatus( int b ) {
		this.status = b;
		this.updateText();
	}
}
