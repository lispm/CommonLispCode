#||

/*
 * Zahlraten
 * Version 1.0
 * 2011-11-30
 * Hans-Peter Habelitz
 */

package ratespiel;

import java.util.Random;
import javax.swing.JOptionPane;

public class Zahlenraten {
    private int zufallszahl;
    private int ratezahl;
    
    void Zahlraten() {
        ratezahl = -1;
    }
    
    void setZufallszahl(int z) {
        zufallszahl = z;
    }
       
    int getZufallszahl() {
        return zufallszahl;
    }
       
    void setRatezahl(int z) {
        ratezahl = z;
    }
       
    int getRatezahl() {
        return ratezahl;
    }
    
    public static void main(String[] args) {
        Zahlenraten spiel;
        spiel = new Zahlenraten();
        boolean geraten = false;
        int versuchzaehler = 0;
        JOptionPane.showMessageDialog(null,
                    "Erraten Sie eine ganze Zahl aus dem Bereich von 1 bis 100!");
        Random randomGenerator = new Random();
        spiel.setZufallszahl(randomGenerator.nextInt(101));
        while (!geraten) {
            spiel.setRatezahl(Integer.parseInt( JOptionPane.showInputDialog("Welche Zahl wird gesucht?")));
            versuchzaehler++;
            if (spiel.getRatezahl() < spiel.getZufallszahl()) {
                JOptionPane.showMessageDialog(null, "Ihre Zahl ist zu klein!");
            } else {
                if (spiel.getRatezahl() > spiel.getZufallszahl()) {
                    JOptionPane.showMessageDialog(null, "Ihre Zahl ist zu groß!");
                } else {
                    geraten = true;
                    JOptionPane.showMessageDialog(null,
                            "Glueckwunsch! Sie haben die Zahl mit "
                            + versuchzaehler + " Versuchen erraten!");
                    }
            } 
        }
    }   
}

||#


;;; Zahlraten in LispWorks
;;  Version 1.0
;;  2012-07-15
;;  Rainer Joswig

(defpackage "RATESPIEL" (:use "COMMON-LISP" "CAPI"))

(in-package "RATESPIEL")

(defun spielen (&aux (zufallszahl (1+ (random 100 (make-random-state t)))))
  (display-message "Erraten Sie eine ganze Zahl aus dem Bereich von 1 bis 100!")
  (loop for versuchzaehler from 1
        for ratezahl = (prompt-for-integer "Welche Zahl wird gesucht?" :min 1 :max 100) 
        when (< ratezahl zufallszahl)
        do (display-message "Ihre Zahl ist zu klein!")
        else if (> ratezahl zufallszahl)
        do (display-message "Ihre Zahl ist zu gross!")
        else
        do (display-message "Glückwunsch! Sie haben die Zahl mit ~a Versuchen erraten" versuchzaehler)
        and do (return)))


