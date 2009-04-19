

import java.awt.event.ActionEvent;
import java.util.ArrayList;


public class ControlButtonListener implements java.awt.event.ActionListener {

	
	GUI g;
	
	public ControlButtonListener( GUI g ) {
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
		System.out.println("Shutting down Process. Aborting Children With Gusto");
		this.g.close();
	}
	
	private void start() {
		System.out.println("Checking For Badassary");
		if ( this.g.loaded ) {
			this.g.primed = true;
			System.out.println("Tis all good in the hood. Primed and Ready for the Controller");
			System.out.println("");
		} else {
			System.out.println("");
			System.err.println("Dude! You need to Init the Lists before you can Run!");
		}
		System.out.println("");
	}
	
	private void init() {
		System.out.println("Creating Passable ArrayList of Agents!");
		ArrayList<AgentSpecs>  as = new ArrayList<AgentSpecs>();
		ArrayList<ObjectSpecs> os = new ArrayList<ObjectSpecs>();
		
		ArrayList<GUIObject>  go = this.g.objects;
		
		for ( int i = 0; i < go.size(); ++i ) {
			if ( go.get(i).getStatus() == 1 ) {
				as.add(go.get(i).a);
//				System.out.println("Agent at " + go.get(i).a.getX() + " " + go.get(i).a.getY() );
//				System.out.println("Sensor Range: " + go.get(i).a.getSensorRange() );
			}
			if ( go.get(i).getStatus() == 2 ) {
				os.add(go.get(i).o);
//				System.out.println("Object at " + go.get(i).o.getX() + " " + go.get(i).o.getY() );
//				System.out.println("Object Status: " + go.get(i).o.getType() );
			}
		}
		
		this.g.as = as;
		this.g.os = os;
		
		this.g.loaded = true;
		
		System.out.println("Created Agent Array. Size: " + as.size() );
		System.out.println("Created Object Array. Size: " + os.size() );
		System.out.println("Objects are Written to GUI. Ready for teh Bay Boy to werk.");
		System.out.println("");
	}
}
