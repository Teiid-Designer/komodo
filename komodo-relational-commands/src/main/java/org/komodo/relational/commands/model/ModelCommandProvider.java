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
package org.komodo.relational.commands.model;

import java.util.HashSet;
import java.util.Set;
import org.komodo.relational.model.Model;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A shell command provider for Models.
 */
public class ModelCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for Model shell commands.
     */
    public ModelCommandProvider() {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#provideCommands()
     */
    @Override
    public Set< Class< ? extends ShellCommand > > provideCommands() {
        final Set< Class< ? extends ShellCommand > > result = new HashSet< >();

        result.add( AddPushdownFunctionCommand.class );
        result.add( AddSourceCommand.class );
        result.add( AddStoredProcedureCommand.class );
        result.add( AddTableCommand.class );
        result.add( AddUserDefinedFunctionCommand.class );
        result.add( AddViewCommand.class );
        result.add( AddVirtualProcedureCommand.class );

        result.add( DeletePushdownFunctionCommand.class );
        result.add( DeleteSourceCommand.class );
        result.add( DeleteStoredProcedureCommand.class );
        result.add( DeleteTableCommand.class );
        result.add( DeleteUserDefinedFunctionCommand.class );
        result.add( DeleteViewCommand.class );
        result.add( DeleteVirtualProcedureCommand.class );

        result.add( SetModelPropertyCommand.class );
        result.add( UnsetModelPropertyCommand.class );
        result.add( ExportCommand.class );
        result.add( ImportCommand.class );

        result.add( ShowPushdownFunctionsCommand.class );
        result.add( ShowSourcesCommand.class );
        result.add( ShowStoredProceduresCommand.class );
        result.add( ShowTablesCommand.class );
        result.add( ShowUserDefinedFunctionsCommand.class );
        result.add( ShowViewsCommand.class );
        result.add( ShowVirtualProceduresCommand.class );

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject)
     */
    @SuppressWarnings( "unchecked" )
    @Override
    public Model resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(Model.RESOLVER.resolvable(uow, kObj)) {
            return Model.RESOLVER.resolve(uow, kObj);
        }
        return null;
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
    public void initWorkspaceState(WorkspaceStatus wsStatus) {
        // Init any workspace state
    }

}
