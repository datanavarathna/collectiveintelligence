

import javax.swing.JButton;

public class ConfigGUIButton extends JButton {
	private static final long serialVersionUID = 1L;

	GUIObject g;
	ConfigGUI c;
	
	public ConfigGUIButton( String s, GUIObject g, ConfigGUI c ) {
		super(s);
		this.g = g;
		this.c = c;
	}
	
	public GUIObject getSuperparentGUI() {
		return this.g;
	}
	public ConfigGUI getParentGUI() {
		return this.c;
	}
}
