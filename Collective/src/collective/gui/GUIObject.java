package collective.gui;

import javax.swing.JLabel;


public class GUIObject extends JLabel {
	private static final long serialVersionUID = 1L;
	boolean status;
	
	GUIObject( boolean status ) {
		super("");
		this.status = status;
		this.updateText();
	}
	
	private void updateText() {
		String labelText;
		
		if ( this.status ) {
			labelText = "Agent";
		} else {
			labelText = "";
		}
		this.setText(labelText);
	}
	
	public boolean getStatus() { return this.status; }
	
	public void setStatus( boolean b ) {
		this.status = b;
		this.updateText();
	}
}
