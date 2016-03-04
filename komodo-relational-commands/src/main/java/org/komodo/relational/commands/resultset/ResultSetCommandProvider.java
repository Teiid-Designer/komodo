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
package org.komodo.relational.commands.resultset;

import java.util.Collections;
import java.util.Set;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A {@link ShellCommandProvider command provider} for common commands of {@link ProcedureResultSet}.
 */
public class ResultSetCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider.
     */
    public ResultSetCommandProvider() {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#getStatusMessage(org.komodo.shell.api.WorkspaceStatus)
     */
    @Override
    public String getStatusMessage( final WorkspaceStatus wsStatus ) {
        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#initWorkspaceState(org.komodo.shell.api.WorkspaceStatus)
     */
    @Override
    public void initWorkspaceState( final WorkspaceStatus wsStatus ) {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#provideCommands()
     */
    @Override
    public Set< Class< ? extends ShellCommand > > provideCommands() {
        return Collections.singleton( ResultSetRenameCommand.class );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject)
     */
    @SuppressWarnings( "unchecked" )
    @Override
    public ProcedureResultSet resolve( final Repository.UnitOfWork uow,
                                       final KomodoObject kobject ) throws KException {
        if ( DataTypeResultSet.RESOLVER.resolvable( uow, kobject ) ) {
            return DataTypeResultSet.RESOLVER.resolve( uow, kobject );
        }

        if ( TabularResultSet.RESOLVER.resolvable( uow, kobject ) ) {
            return TabularResultSet.RESOLVER.resolve( uow, kobject );
        }

        return null;
    }

}
