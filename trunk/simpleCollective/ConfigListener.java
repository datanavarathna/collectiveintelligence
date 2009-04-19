package collective.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.JTextField;

public class ConfigListener implements ActionListener {

	@Override
	public void actionPerformed(ActionEvent e) {
//		System.out.println(e.getActionCommand());
//		System.out.println(e.getSource());
		
		if (e.getSource() instanceof ConfigGUIButton ) {
			ConfigGUIButton b = (ConfigGUIButton) e.getSource();
			ConfigGUI c = b.getParentGUI();
			ArrayList<JTextField> olist = c.o;
			
			for ( int i = 0; i < olist.size(); ++i ) {
				System.out.println("Updating " + olist.get(i).getName() + " With " + olist.get(i).getText() );
				try {
					b.g.updateAttr(olist.get(i).getName(), olist.get(i).getText() );
				} catch ( NumberFormatException e1 ) {
					System.err.println("Caught an Error with " + olist.get(i).getName() );
					System.err.println("Exception: " + e1 );
					System.err.println("This Exception can be ignored, but I'm Not changing Values!");
				}
			}
			
			c.p.dispose();
			
		} else {
			System.out.println("Unknown Signal. Ignoring.");
		}
	}

}
