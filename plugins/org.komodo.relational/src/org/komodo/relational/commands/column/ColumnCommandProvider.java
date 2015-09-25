/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.column;

import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.internal.ColumnImpl;
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
    public Map< String, Class< ? extends ShellCommand >> provideCommands() {
        final Map< String, Class< ? extends ShellCommand >> result = new HashMap<>();

        result.put( SetColumnPropertyCommand.NAME, SetColumnPropertyCommand.class );

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
        if(ColumnImpl.RESOLVER.resolvable(uow, kObj)) {
            return ColumnImpl.RESOLVER.resolve(uow, kObj);
        }
        return null;
    }

    @Override
    public boolean isRoot ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(ColumnImpl.RESOLVER.resolvable(uow, kObj)) {
            return false;
        }
        return false;
    }

    @Override
    public String getTypeDisplay ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(ColumnImpl.RESOLVER.resolvable(uow, kObj)) {
            Column column = ColumnImpl.RESOLVER.resolve(uow, kObj);
            return column.getTypeDisplayName();
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
