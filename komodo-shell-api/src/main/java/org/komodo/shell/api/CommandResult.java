/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
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
         * @see org.komodo.shell.api.CommandResult#getDetail(java.lang.String)
         */
        @Override
        public Object getDetail( final String key ) {
            return null;
        }

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
         * @see org.komodo.shell.api.CommandResult#getDetail(java.lang.String)
         */
        @Override
        public Object getDetail( final String key ) {
            return null;
        }

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
     * @param key
     *        the identifier for the detail being requested (cannot be empty)
     * @return the result detail associated with the specified key (can be <code>null</code>)
     */
    Object getDetail( final String key );

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
