package collective.gui;

import java.awt.GridLayout;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTextField;

public class ConfigGUI {
	
	GUIObject g;
	JFrame    p;
	
	ConfigListener l;
	
	int x;
	int y;
	
	ConfigGUI( GUIObject g ) {
		this.g = g;
		this.l = new ConfigListener();
	}
	
	public void show( int x, int y ) {

		this.x = x;
		this.y = y;
		
		if ( this.g.getStatus() == 1 ) {
			this.agentConf();
		} else if ( this.g.getStatus() == 2 ) {
			this.objectConf();
		} else {
			System.err.println("No Config for Nothing :X PUKE!");
		}
		
		this.addSubmitButton();
		
		
	}

	private void addSubmitButton() {
		JButton t = new JButton();
		t.setText("Update");
		t.setVisible(true);
		t.addActionListener( this.l );
		this.p.add(t);
		
		JLabel l = new JLabel("Make Babies");
		l.setVisible(true);
		this.p.add(l);
	}

	public void setup() {
		this.p = new JFrame();
		this.p.setSize( this.x, this.y );
		this.p.setVisible(true);
		this.p.setLayout( new GridLayout ( 5, 2 ) ) ;
	}
	
	public void objectConf() {
		this.setup();
		
		JTextField t = new JTextField();
		t.setVisible(true);
		t.setName("status");
		t.addActionListener( this.l );
		this.p.add(t);
		
		JLabel l = new JLabel("Status");
		l.setVisible(true);
		this.p.add(l);
	}
	
	public void agentConf() {
		this.setup();
		
		JTextField t = new JTextField();
		t.setName("srange");
		t.setVisible(true);
		t.addActionListener( this.l );
		this.p.add(t);
		
		JLabel l = new JLabel("Sensor Range");
		l.setVisible(true);
		this.p.add(l);
		
		t = new JTextField();
		t.setName("sdeltrange");
		t.setVisible(true);
		t.addActionListener( this.l );
		this.p.add(t);
		
		l = new JLabel("Sensor Delta Range");
		l.setVisible(true);
		this.p.add(l);
		
		t = new JTextField();
		t.setVisible(true);
		t.setName("dangle");
		t.addActionListener( this.l );
		this.p.add(t);
		
		l = new JLabel("Delta Angle");
		l.setVisible(true);
		this.p.add(l);
		
		t = new JTextField();
		t.setVisible(true);
		t.setName("magic");
		t.addActionListener( this.l );
		this.p.add(t);
		
		l = new JLabel("Magic / More Magic");
		l.setVisible(true);
		this.p.add(l);
	}
	
}
