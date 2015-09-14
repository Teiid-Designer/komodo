/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.vdb;

import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.VdbImpl;
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
    public Map< String, Class< ? extends ShellCommand >> provideCommands() {
        final Map< String, Class< ? extends ShellCommand >> result = new HashMap<>();

        result.put( AddDataRoleCommand.NAME, AddDataRoleCommand.class );
        result.put( DeleteDataRoleCommand.NAME, DeleteDataRoleCommand.class );
        result.put( ShowDataRolesCommand.NAME, ShowDataRolesCommand.class );

        result.put( AddEntryCommand.NAME, AddEntryCommand.class );
        result.put( DeleteEntryCommand.NAME, DeleteEntryCommand.class );
        result.put( ShowEntriesCommand.NAME, ShowEntriesCommand.class );

        result.put( AddImportCommand.NAME, AddImportCommand.class );
        result.put( DeleteImportCommand.NAME, DeleteImportCommand.class );
        result.put( ShowImportsCommand.NAME, ShowImportsCommand.class );

        result.put( AddModelCommand.NAME, AddModelCommand.class );
        result.put( DeleteModelCommand.NAME, DeleteModelCommand.class );
        result.put( ShowModelsCommand.NAME, ShowModelsCommand.class );

        result.put( AddTranslatorCommand.NAME, AddTranslatorCommand.class );
        result.put( DeleteTranslatorCommand.NAME, DeleteTranslatorCommand.class );
        result.put( ShowTranslatorsCommand.NAME, ShowTranslatorsCommand.class );

        result.put( ExportCommand.NAME, ExportCommand.class );
        result.put( SetVdbPropertyCommand.NAME, SetVdbPropertyCommand.class );
        result.put( ShowVdbCommand.NAME, ShowVdbCommand.class );

        return result;
    }
    
    @Override
    public Vdb resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(VdbImpl.RESOLVER.resolvable(uow, kObj)) {
            return VdbImpl.RESOLVER.resolve(uow, kObj);
        }
        return null;
    }
    
    @Override
    public boolean isRoot ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(VdbImpl.RESOLVER.resolvable(uow, kObj)) {
            return false;
        }
        return false;
    }
    
    @Override
    public String getTypeDisplay ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(VdbImpl.RESOLVER.resolvable(uow, kObj)) {
            Vdb vdb = VdbImpl.RESOLVER.resolve(uow, kObj);
            return vdb.getTypeDisplayName();
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
