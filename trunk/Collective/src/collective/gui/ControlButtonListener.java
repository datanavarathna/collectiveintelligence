package collective.gui;

import java.awt.event.ActionEvent;
import java.util.ArrayList;


public class ControlButtonListener implements java.awt.event.ActionListener {

	
	gui g;
	
	public ControlButtonListener( gui g ) {
		this.g = g;
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		String a = e.getActionCommand();
		
		if ( a.equals("kill" ) ) {
			this.kill();
		} else if ( a.equals("initialize" ) ) {
			this.init();
		} else if ( a.equals("start" ) ) {
			this.start();
		} else {
			System.err.println("Controller rcvd a command that I done");
			System.err.println("gone and not understood. I am doin nothing.");
		}
	}
	
	private void kill() {
		System.out.println("God said I need to kill.");
		System.out.println("Shutting down Process. Aborting Children");
	}
	
	private void start() {
		System.out.println("Starting Application!");
	}
	
	private void init() {
		System.out.println("Creating Passable ArrayList of Agents!");
		ArrayList<AgentSpecs>  as = new ArrayList<AgentSpecs>();
		ArrayList<ObjectSpecs> os = new ArrayList<ObjectSpecs>();
		
		ArrayList<GUIObject>  go = this.g.objects;
		
		for ( int i = 0; i < go.size(); ++i ) {
			if ( go.get(i).getStatus() == 1 ) {
				as.add(go.get(i).a);
			}
			if ( go.get(i).getStatus() == 2 ) {
				os.add(go.get(i).o);
			}
		}
		
		this.g.as = as;
		this.g.os = os;
		
		System.out.println("Created Agent Array. Size: " + as.size() );
		System.out.println("Created Object Array. Size: " + os.size() );
		
		
	}
}
