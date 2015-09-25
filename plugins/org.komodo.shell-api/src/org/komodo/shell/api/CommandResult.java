/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.api;

/**
 * Represents a {@link ShellCommand} result.
 */
public interface CommandResult {

    /**
     * A result, with no message and no error, indicating the command failed.
     */
    CommandResult FAIL = new CommandResult() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.api.CommandResult#getError()
         */
        @Override
        public Exception getError() {
            return null;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.api.CommandResult#getMessage()
         */
        @Override
        public String getMessage() {
            return null;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.api.CommandResult#isOk()
         */
        @Override
        public boolean isOk() {
            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.api.CommandResult#isPersistable()
         */
        @Override
        public boolean isPersistable() {
            return true;
        }

    };

    /**
     * A result, with no message, indicating the command succeeded.
     */
    CommandResult SUCCESS = new CommandResult() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.api.CommandResult#getError()
         */
        @Override
        public Exception getError() {
            return null;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.api.CommandResult#getMessage()
         */
        @Override
        public String getMessage() {
            return null;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.api.CommandResult#isOk()
         */
        @Override
        public boolean isOk() {
            return true;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.api.CommandResult#isPersistable()
         */
        @Override
        public boolean isPersistable() {
            return true;
        }

    };

    /**
     * @return the error or <code>null</code> if no error occurred
     */
    Exception getError();

    /**
     * @return the error or OK message (can be empty if nothing should be printed after command execution completes
     */
    String getMessage();

    /**
     * @return <code>true</code> if the command execution was successful
     */
    boolean isOk();

    /**
     * @return <code>true</code> if a successful command execution must be committed or if a failed execution must be rolled back
     */
    boolean isPersistable();

}
