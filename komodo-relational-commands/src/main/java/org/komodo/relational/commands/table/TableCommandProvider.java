/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.table;

import java.util.HashSet;
import java.util.Set;
import org.komodo.relational.model.Table;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A shell command provider for Tables.
 */
public class TableCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for Table shell commands.
     */
    public TableCommandProvider() {
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

        result.add( AddAccessPatternCommand.class );
        result.add( AddColumnCommand.class );
        result.add( AddForeignKeyCommand.class );
        result.add( AddIndexCommand.class );
        result.add( AddUniqueConstraintCommand.class );
        result.add( AddPrimaryKeyCommand.class );

        result.add( DeleteAccessPatternCommand.class );
        result.add( DeleteColumnCommand.class );
        result.add( DeleteForeignKeyCommand.class );
        result.add( DeleteIndexCommand.class );
        result.add( DeleteUniqueConstraintCommand.class );
        result.add( DeletePrimaryKeyCommand.class );

        result.add( SetTablePropertyCommand.class );
        result.add( UnsetTablePropertyCommand.class );

        result.add( ShowAccessPatternsCommand.class );
        result.add( ShowColumnsCommand.class );
        result.add( ShowIndexesCommand.class );
        result.add( ShowUniqueConstraintsCommand.class );

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
    public Table resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(Table.RESOLVER.resolvable(uow, kObj)) {
            return Table.RESOLVER.resolve(uow, kObj);
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
