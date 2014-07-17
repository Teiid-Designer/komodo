package org.komodo.spi.outcome;

import java.util.List;


/**
 * The return state of a process or function
 */
public interface IOutcome {

    public enum Level {
        OK, INFO, WARNING, ERROR;
    }

    List<IOutcome> getOutcomes();
    
    void addOutcome(IOutcome outcome);
    
    void addOutcomes(List<IOutcome> outcomes);
    
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
