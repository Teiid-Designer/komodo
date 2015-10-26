/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.column;

import java.util.HashSet;
import java.util.Set;
import org.komodo.relational.model.Column;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A shell command provider for Columns.
 */
public class ColumnCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for Table shell commands.
     */
    public ColumnCommandProvider() {
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

        result.add( SetColumnPropertyCommand.class );
        result.add( UnsetColumnPropertyCommand.class );
        result.add( RenameChildCommand.class );

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
    public Column resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(Column.RESOLVER.resolvable(uow, kObj)) {
            return Column.RESOLVER.resolve(uow, kObj);
        }
        return null;
    }

    @Override
    public String getTypeDisplay ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        final Column resolved = resolve( uow, kObj );
        return ( ( resolved == null ) ? null : resolved.getTypeDisplayName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#getStatusMessage(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public String getStatusMessage ( final Repository.UnitOfWork uow, final KomodoObject kObj ) {
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
