/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
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
        result.add( ExportCommand.class );
        result.add( UnsetVdbPropertyCommand.class );

        result.add( UploadModelCommand.class );

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
     * @see org.komodo.shell.api.ShellCommandProvider#getTypeDisplay(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public String getTypeDisplay ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        final Vdb resolved = resolve( uow, kObj );
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
