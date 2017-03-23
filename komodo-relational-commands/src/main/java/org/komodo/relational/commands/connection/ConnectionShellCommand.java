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
package org.komodo.relational.commands.connection;

import java.util.Arrays;
import java.util.List;

import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.connection.Connection;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link Connection}-related shell commands.
 */
abstract class ConnectionShellCommand extends RelationalShellCommand {

    protected static final String JNDI_NAME = "jndiName"; //$NON-NLS-1$
    protected static final String DESCRIPTION = "description"; //$NON-NLS-1$
    protected static final String EXT_LOC = "externalLocation"; //$NON-NLS-1$
    protected static final String DRIVER_NAME = "driverName"; //$NON-NLS-1$
    protected static final String CLASS_NAME = "className"; //$NON-NLS-1$
    protected static final String JDBC = "jdbc"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { DESCRIPTION, EXT_LOC, JNDI_NAME, DRIVER_NAME, CLASS_NAME, JDBC } );

    protected ConnectionShellCommand( final String name,
                                  final WorkspaceStatus status ) {
        super( status, name );
    }

    protected Connection getConnection() throws Exception {
        assert getContext() instanceof Connection;
        return Connection.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return Connection.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}
