/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell;

import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;

/**
 * Represents a result of a {@link ShellCommand command} execution.
 */
public class CommandResultImpl implements CommandResult {

    private final Exception error;
    private final String message;
    private boolean persistable;
    private final boolean success;

    /**
     * Constructs a successful, persistable result.
     *
     * @param success
     *        <code>true</code> if the command successfully executed
     * @param message
     *        the error or OK message (can be empty)
     * @param error
     *        the error that occurred or <code>null</code>
     */
    public CommandResultImpl( final boolean success,
                              final String message,
                              final Exception error ) {
        this.success = success;
        this.message = message;
        this.persistable = true;
        this.error = error;
    }

    /**
     * Constructs a successful, persistable result with the specified message.
     *
     * @param message
     *        the error or OK message (can be empty)
     */
    public CommandResultImpl( final String message ) {
        this( true, message, null );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.CommandResult#getError()
     */
    @Override
    public Exception getError() {
        return this.error;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.CommandResult#getMessage()
     */
    @Override
    public String getMessage() {
        return this.message;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.CommandResult#isOk()
     */
    @Override
    public boolean isOk() {
        return this.success;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.CommandResult#isPersistable()
     */
    @Override
    public boolean isPersistable() {
        return this.persistable;
    }

    /**
     * @param newPersistable
     *        <code>true</code> if the current transaction should be committed or rolled back
     */
    public void setPersistable( final boolean newPersistable ) {
        this.persistable = newPersistable;
    }

}
