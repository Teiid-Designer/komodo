package org.komodo.spi.outcome;

import java.util.List;


/**
 * The return state of a process or function
 */
public interface Outcome {

    public enum Level {
        OK, INFO, WARNING, ERROR;
    }

    List<Outcome> getOutcomes();
    
    void addOutcome(Outcome outcome);
    
    void addOutcomes(List<Outcome> outcomes);
    
    /**
     * @return
     */
    String getMessage();

    /**
     * @return
     */
    Exception getException();

    /**
     * @return
     */
    Level getLevel();

    /**
     * @return
     */
    boolean isOK();
    
    boolean isMultiOutcome();

}
