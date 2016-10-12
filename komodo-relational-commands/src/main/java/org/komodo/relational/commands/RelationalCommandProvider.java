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
package org.komodo.relational.commands;

import java.util.HashSet;
import java.util.Set;

import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A shell command provider for general relational commands.
 */
public class RelationalCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for general relational shell commands.
     */
    public RelationalCommandProvider() {
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

        result.add( FindCommand.class );
        result.add( SetCustomPropertyCommand.class );
        result.add( UnsetCustomPropertyCommand.class );
        result.add( UnsetCustomOptionCommand.class );
        result.add( SetCustomOptionCommand.class );

        // These commands disable basic shell commands
        result.add( RelationalAddChildCommand.class );
        result.add( RelationalDeleteChildCommand.class );
        result.add( RelationalAddDescriptorCommand.class );
        result.add( RelationalRemoveDescriptorCommand.class );
        result.add( RelationalSetPrimaryTypeCommand.class );
        result.add( RelationalShowDescriptorsCommand.class );

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
    public WorkspaceManager resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(kObj.getRepository(), uow);
        if(wkspMgr.getAbsolutePath().equals(kObj.getAbsolutePath())) {
            return wkspMgr;
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
     * @throws KException the exception
     */
    /* (non-Javadoc)
     * @see org.komodo.shell.api.ShellCommandProvider#initWorkspaceState(org.komodo.shell.api.WorkspaceStatus)
     */
    @Override
    public void initWorkspaceState(WorkspaceStatus wsStatus) throws KException {
        // Init any workspace state
    }

}
