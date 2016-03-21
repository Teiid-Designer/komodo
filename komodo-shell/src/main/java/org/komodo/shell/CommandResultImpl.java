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
package org.komodo.shell;

import java.util.HashMap;
import java.util.Map;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
import org.komodo.utils.ArgCheck;

/**
 * Represents a result of a {@link ShellCommand command} execution.
 */
public class CommandResultImpl implements CommandResult {

    private Map< String, Object > detailMap;
    private final Exception error;
    private final String message;
    private boolean persistable;
    private final boolean success;

    /**
     * Constructs a persistable result.
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
        this.error = error;
        this.persistable = true;
    }

    /**
     * Constructs a failure, persistable result with no message. The error message will be used as the result message.
     *
     * @param error
     *        the error that occurred (cannot be <code>null</code>)
     */
    public CommandResultImpl( final Exception error ) {
        this( false, null, error );
        ArgCheck.isNotNull( error, "error" ); //$NON-NLS-1$
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
     * @param key
     *        the detail identifier (cannot be empty)
     * @param detail
     *        the detail object (can be <code>null</code>)
     */
    public void addDetail( final String key,
                           final Object detail ) {
        ArgCheck.isNotEmpty( key, "key" ); //$NON-NLS-1$

        if ( this.detailMap == null ) {
            this.detailMap = new HashMap< >();
        }

        this.detailMap.put( key, detail );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.CommandResult#getDetail(java.lang.String)
     */
    @Override
    public Object getDetail( final String key ) {
        if ( this.detailMap == null ) {
            return null;
        }

        return this.detailMap.get( key );
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
     * @param key
     *        the identifier of the detail being removed (cannot be empty)
     * @return the detail being removed (can be <code>null</code>)
     */
    public Object removeDetail( final String key ) {
        ArgCheck.isNotEmpty( key, "key" ); //$NON-NLS-1$

        if ( this.detailMap == null ) {
            return null;
        }

        final Object removed = this.detailMap.remove( key );

        if ( this.detailMap.isEmpty() ) {
            this.detailMap = null;
        }

        return removed;
    }

    /**
     * @param newPersistable
     *        <code>true</code> if the current transaction should be committed or rolled back
     */
    public void setPersistable( final boolean newPersistable ) {
        this.persistable = newPersistable;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        final StringBuffer buff = new StringBuffer( );

        if(this.isOk()) {
            buff.append("isOK : true");  //$NON-NLS-1$
        } else {
            buff.append("isOk : false");  //$NON-NLS-1$
            buff.append(" msg  : " + getMessage());  //$NON-NLS-1$
        }
        return buff.toString();
    }

}
