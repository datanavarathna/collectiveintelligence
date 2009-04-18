package collective.gui;

import java.awt.event.ActionEvent;


public class ControlButtonListener implements java.awt.event.ActionListener {

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
		System.out.println("Create and Pass in the Agents to be created");
	}
	
	

}
