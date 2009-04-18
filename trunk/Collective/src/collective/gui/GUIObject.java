package collective.gui;

import java.awt.Color;

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
			this.setBackground(new Color(225,225,225));
		} else if ( this.status == 1 ) {
			labelText = "Agent Here";
			this.setBackground(new Color(224,61,61));
		} else if ( this.status == 2 ) {
			labelText = "Object Here";
			this.setBackground(new Color(4,115,131));
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
