

import java.awt.GridLayout;
import java.util.ArrayList;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTextField;

public class ConfigGUI {

	GUIObject g;
	JFrame    p;

	ConfigListener l;

	ArrayList<JTextField> o;

	int x;
	int y;

	ConfigGUI( GUIObject g ) {
		this.g = g;
		this.l = new ConfigListener();
		this.o = new ArrayList<JTextField>();
	}

	public void show( int x, int y ) {

		this.x = x;
		this.y = y;

		if ( this.g.getStatus() == 1 ) {
			this.agentConf();
			this.addSubmitButton();
		} else if ( this.g.getStatus() == 2 ) {
			this.objectConf();
			this.addSubmitButton();
		} else {
			// System.err.println("Can't configure an empty Square.");
		}
	}

	private void addSubmitButton() {
		ConfigGUIButton t = new ConfigGUIButton( "" ,this.g, this );

		this.p.add(t);
		t.setText("Update");
		t.setVisible(true);
		t.addActionListener( this.l );

		JLabel l = new JLabel("Update Object");
		l.setVisible(true);
		this.p.add(l);
	}

	public void setup() {
		this.p = new JFrame();
		this.p.setSize( this.x, this.y );
		this.p.setVisible(true);
	}

	public void objectConf() {
		this.setup();

		this.p.setLayout( new GridLayout ( 2, 2 ) ) ;

		JTextField t = new JTextField();
		t.setVisible(true);
		t.setName("status");
		t.addActionListener( this.l );
		t.setText(String.valueOf(this.g.o.getType()));
		this.p.add(t);
		this.o.add(t);

		JLabel l = new JLabel("Status");
		l.setVisible(true);
		this.p.add(l);
	}

	public void agentConf() {
		this.setup();

		this.p.setLayout( new GridLayout ( 5, 2 ) ) ;

		JTextField t = new JTextField();
		t.setName("srange");
		t.setText(String.valueOf(this.g.a.getSensorRange()));
		t.setVisible(true);
		t.addActionListener( this.l );
		this.p.add(t);
		this.o.add(t);

		JLabel l = new JLabel("Sensor Range");
		l.setVisible(true);
		this.p.add(l);

		t = new JTextField();
		t.setName("sdeltrange");
		t.setText(String.valueOf(this.g.a.getSensorDeltaRange()));
		t.setVisible(true);
		t.addActionListener( this.l );
		this.p.add(t);
		this.o.add(t);

		l = new JLabel("Sensor Delta Range");
		l.setVisible(true);
		this.p.add(l);

		t = new JTextField();
		t.setVisible(true);
		t.setName("dangle");
		t.setText(String.valueOf(this.g.a.getDeltaAngle()));
		t.addActionListener( this.l );
		this.p.add(t);
		this.o.add(t);

		l = new JLabel("Delta Angle");
		l.setVisible(true);
		this.p.add(l);

		l = new JLabel("X: " + String.valueOf(this.g.a.getX()) );
		l.setVisible(true);
		this.p.add(l);

		l = new JLabel("Y: " + String.valueOf(this.g.a.getY()) );
		l.setVisible(true);
		this.p.add(l);
	}

}
