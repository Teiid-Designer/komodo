package org.komodo.spi.outcome;


/**
 * The return state of a process or function
 */
public interface IOutcome {

    public enum Level {
        OK, INFO, WARNING, ERROR;
    }

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

}
