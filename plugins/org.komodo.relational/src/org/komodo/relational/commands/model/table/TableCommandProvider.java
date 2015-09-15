/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.model.table;

import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.internal.TableImpl;
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

        return result;
    }
    
    @Override
    public Table resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(TableImpl.RESOLVER.resolvable(uow, kObj)) {
            return TableImpl.RESOLVER.resolve(uow, kObj);
        }
        return null;
    }
    
    @Override
    public boolean isRoot ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(TableImpl.RESOLVER.resolvable(uow, kObj)) {
            return false;
        }
        return false;
    }
    
    @Override
    public String getTypeDisplay ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(TableImpl.RESOLVER.resolvable(uow, kObj)) {
            Table table = TableImpl.RESOLVER.resolve(uow, kObj);
            return table.getTypeDisplayName();
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
