/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.workspace;

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
 * A shell command provider for workspace-level commands.
 */
public class WorkspaceCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for workspace shell commands.
     */
    public WorkspaceCommandProvider() {
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

        result.add( CreateDatasourceCommand.class );
        result.add( CreateSchemaCommand.class );
        result.add( CreateVdbCommand.class );
        result.add( DeleteDatasourceCommand.class );
        result.add( DeleteSchemaCommand.class );
        result.add( DeleteVdbCommand.class );
        result.add( ImportVdbCommand.class );
        result.add( UploadVdbCommand.class );
        result.add( ExportVdbCommand.class );
        result.add( UploadDatasourceCommand.class );
        result.add( ExportDatasourceCommand.class );
        result.add( WorkspaceSetPropertyCommand.class );
        result.add( WorkspaceUnsetPropertyCommand.class );

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
        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(kObj.getRepository());
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
