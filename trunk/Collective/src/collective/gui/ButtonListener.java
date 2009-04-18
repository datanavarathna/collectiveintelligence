package collective.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class ButtonListener implements ActionListener {

	@Override
	public void actionPerformed(ActionEvent e) {
		GUIObject go = (GUIObject) e.getSource();
		
		int status = go.getStatus();
		int sStatus = 0;
		
		if ( status == 0 ) {
			sStatus = 2;
		} else if ( status == 1 ) {
			System.err.println("Agent is On this Block! Unable to Change!");
			sStatus = status;
		} else if ( status == 2 ) {
			sStatus = 0;
		} else {
			sStatus = 0;
		}
		
		go.setStatus( sStatus );
		
		
	}
}
