/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.model;

import java.util.HashSet;
import java.util.Set;
import org.komodo.relational.model.Model;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A shell command provider for Models.
 */
public class ModelCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for Model shell commands.
     */
    public ModelCommandProvider() {
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

        result.add( AddPushdownFunctionCommand.class );
        result.add( AddSourceCommand.class );
        result.add( AddStoredProcedureCommand.class );
        result.add( AddTableCommand.class );
        result.add( AddUserDefinedFunctionCommand.class );
        result.add( AddViewCommand.class );
        result.add( AddVirtualProcedureCommand.class );

        result.add( DeletePushdownFunctionCommand.class );
        result.add( DeleteSourceCommand.class );
        result.add( DeleteStoredProcedureCommand.class );
        result.add( DeleteTableCommand.class );
        result.add( DeleteUserDefinedFunctionCommand.class );
        result.add( DeleteViewCommand.class );
        result.add( DeleteVirtualProcedureCommand.class );

        result.add( SetModelPropertyCommand.class );
        result.add( UnsetModelPropertyCommand.class );
        result.add( ExportCommand.class );
        result.add( ImportCommand.class );

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
    public Model resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(Model.RESOLVER.resolvable(uow, kObj)) {
            return Model.RESOLVER.resolve(uow, kObj);
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
        final Model resolved = resolve( uow, kObj );
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
