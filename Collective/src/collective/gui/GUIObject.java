package collective.gui;

import java.awt.Color;

import javax.swing.JButton;


public class GUIObject extends JButton {
	private static final long serialVersionUID = 1L;
	int status;
	AgentSpecs  a;
	ObjectSpecs o;
	
	GUIObject( int status ) {
		super("");
		this.status = status;
		this.a = new AgentSpecs();
		this.o = new ObjectSpecs();
		
		this.updateText();
	}
	
	private void updateText() {
		String labelText = "";
		
		if ( this.status == 0 ) {
			labelText = "";
			this.setBackground(new Color(225,225,225));
		} else if ( this.status == 1 ) {
			labelText = "Agent Here";
			this.setBackground(new Color(4,115,131));
		} else if ( this.status == 2 ) {
			labelText = "Object Here";
			this.setBackground(new Color(224,61,61));
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

	public void updateAttr(String name, String text) throws NumberFormatException {
		if ( name.equals("srange") ) {
			this.a.setSensorRange(Integer.parseInt(text) );
		} else if ( name.equals("sdeltrange") ) {
			this.a.setSensorDeltaRange(Integer.parseInt(text) );
		} else if ( name.equals("dangle") ) {
			this.a.setDeltaAngle(Integer.parseInt(text));
		} else if ( name.equals("status") ) {
			this.o.setType(Integer.parseInt(text) );
		}
	}
}
