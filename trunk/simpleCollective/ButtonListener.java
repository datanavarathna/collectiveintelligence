

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class ButtonListener implements ActionListener {

	@Override
	public void actionPerformed(ActionEvent e) {
		if ( e.getModifiers() == 16 ) {
			clicked(e);
		} else if ( e.getModifiers() == 18 ) {
			ctrlClicked(e);
		} else if ( e.getModifiers() == 17 ) {
			configNode((GUIObject)e.getSource());
		}
		
	}
	
	private void configNode(GUIObject source) {
		ConfigGUI g = new ConfigGUI( source );
		g.show( 500, 200 );
	}

	private void ctrlClicked( ActionEvent e ) {
		GUIObject go = (GUIObject) e.getSource();
		
		int status = go.getStatus();
		int sStatus = 0;
		
		if ( status == 0 ) {
			sStatus = 2;
		} else if ( status == 1 ) {
			sStatus = 0;
		} else if ( status == 2 ) {
			sStatus = 1;
		} else {
			sStatus = 0;
		}
		
		go.setStatus( sStatus );
	}
	
	private void clicked( ActionEvent e ) {
		GUIObject go = (GUIObject) e.getSource();
		
		int status = go.getStatus();
		int sStatus = 0;
		
		if ( status == 0 ) {
			sStatus = 1;
		} else if ( status == 1 ) {
			sStatus = 2;
		} else if ( status == 2 ) {
			sStatus = 0;
		} else {
			sStatus = 0;
		}
		
		go.setStatus( sStatus );
	}
	
}
