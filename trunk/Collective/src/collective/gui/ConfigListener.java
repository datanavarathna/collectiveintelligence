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
			}
			
			
		} else {
			System.out.println("Unknown Signal. Ignoring.");
		}
	}

}
