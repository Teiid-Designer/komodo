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
package org.komodo.relational.commands.vdb;

import java.util.HashSet;
import java.util.Set;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A shell command provider for VDBs.
 */
public class VdbCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for VDB shell commands.
     */
    public VdbCommandProvider() {
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

        result.add( AddDataRoleCommand.class );
        result.add( AddEntryCommand.class );
        result.add( AddImportCommand.class );
        result.add( AddModelCommand.class );
        result.add( AddTranslatorCommand.class );

        result.add( DeleteDataRoleCommand.class );
        result.add( DeleteEntryCommand.class );
        result.add( DeleteImportCommand.class );
        result.add( DeleteModelCommand.class );
        result.add( DeleteTranslatorCommand.class );

        result.add( ShowDataRolesCommand.class );
        result.add( ShowEntriesCommand.class );
        result.add( ShowImportsCommand.class );
        result.add( ShowModelsCommand.class );
        result.add( ShowTranslatorsCommand.class );

        result.add( ShowVdbCommand.class );
        result.add( SetVdbPropertyCommand.class );
        result.add( UnsetVdbPropertyCommand.class );

        result.add( UploadModelCommand.class );

        result.add( ValidateVdbCommand.class );

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
    public Vdb resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(Vdb.RESOLVER.resolvable(uow, kObj)) {
            return Vdb.RESOLVER.resolve(uow, kObj);
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
