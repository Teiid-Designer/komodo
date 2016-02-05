/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.datarole;

import java.util.HashSet;
import java.util.Set;
import org.komodo.relational.commands.vdb.ShowDataRolesCommand;
import org.komodo.relational.vdb.DataRole;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A shell command provider for {@link DataRole}s.
 */
public class DataRoleCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for {@link DataRole} shell commands.
     */
    public DataRoleCommandProvider() {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#provideCommands()
     */
    @Override
    public Set< Class< ? extends ShellCommand > > provideCommands() {
        final Set< Class< ? extends ShellCommand > > result = new HashSet< Class< ? extends ShellCommand > >();

        result.add( AddMappedRoleCommand.class );
        result.add( AddPermissionCommand.class );
        result.add( DeleteMappedRoleCommand.class );
        result.add( DeletePermissionCommand.class );
        result.add( SetDataRolePropertyCommand.class );
        result.add( ShowDataRolesCommand.class );
        result.add( ShowMappedRolesCommand.class );
        result.add( ShowPermissionsCommand.class );
        result.add( UnsetDataRolePropertyCommand.class );

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
    public DataRole resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(DataRole.RESOLVER.resolvable(uow, kObj)) {
            return DataRole.RESOLVER.resolve(uow, kObj);
        }
        return null;
    }

    /**
     * @throws KException the exception
     */
    @Override
    public String getStatusMessage ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
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
