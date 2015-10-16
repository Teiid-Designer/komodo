/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.table;

import java.util.HashMap;
import java.util.Map;
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
    public Map< String, Class< ? extends ShellCommand >> provideCommands() {
        final Map< String, Class< ? extends ShellCommand >> result = new HashMap<>();

        result.put( AddAccessPatternCommand.NAME, AddAccessPatternCommand.class );
        result.put( AddColumnCommand.NAME, AddColumnCommand.class );
        result.put( AddForeignKeyCommand.NAME, AddForeignKeyCommand.class );
        result.put( AddIndexCommand.NAME, AddIndexCommand.class );
        result.put( AddUniqueConstraintCommand.NAME, AddUniqueConstraintCommand.class );

        result.put( DeleteAccessPatternCommand.NAME, DeleteAccessPatternCommand.class );
        result.put( DeleteColumnCommand.NAME, DeleteColumnCommand.class );
        result.put( DeleteForeignKeyCommand.NAME, DeleteForeignKeyCommand.class );
        result.put( DeleteIndexCommand.NAME, DeleteIndexCommand.class );
        result.put( DeleteUniqueConstraintCommand.NAME, DeleteUniqueConstraintCommand.class );

        result.put( SetTablePropertyCommand.NAME, SetTablePropertyCommand.class );
        result.put( UnsetTablePropertyCommand.NAME, UnsetTablePropertyCommand.class );

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
     * @see org.komodo.shell.api.ShellCommandProvider#getTypeDisplay(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public String getTypeDisplay ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        final Table resolved = resolve( uow, kObj );
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
