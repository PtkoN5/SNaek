import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

public class wm {
    public static void main(String[] args) {
        //JFrame create
        JFrame frame = new JFrame("WindowManager");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(300, 200);

        //Create JPanel
        JPanel panel = new JPanel();

        //create new button
        JButton button = new JButton("testbutton");

        //add actionlistener button
        button.addActionListener((actionEvent) -> {
            //Message when clicked
            JOptionPane.showMessageDialog(frame, "klick klappt");
        });

        //adding the button to the panel
        panel.add(button);

        // adding the panel to the frame
        frame.add(panel);

        //set the frame to be visible
        frame.setVisible(true);
    }
}