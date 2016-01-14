/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.resultset;

import java.util.Collections;
import java.util.Set;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A {@link ShellCommandProvider command provider} for common commands of {@link ProcedureResultSet}.
 */
public class ResultSetCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider.
     */
    public ResultSetCommandProvider() {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#getStatusMessage(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public String getStatusMessage( final Repository.UnitOfWork uow,
                                    final KomodoObject kobject ) {
        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#initWorkspaceState(org.komodo.shell.api.WorkspaceStatus)
     */
    @Override
    public void initWorkspaceState( final WorkspaceStatus wsStatus ) {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#provideCommands()
     */
    @Override
    public Set< Class< ? extends ShellCommand > > provideCommands() {
        return Collections.singleton( ResultSetRenameCommand.class );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject)
     */
    @SuppressWarnings( "unchecked" )
    @Override
    public ProcedureResultSet resolve( final Repository.UnitOfWork uow,
                                       final KomodoObject kobject ) throws KException {
        if ( DataTypeResultSet.RESOLVER.resolvable( uow, kobject ) ) {
            return DataTypeResultSet.RESOLVER.resolve( uow, kobject );
        }

        if ( TabularResultSet.RESOLVER.resolvable( uow, kobject ) ) {
            return TabularResultSet.RESOLVER.resolve( uow, kobject );
        }

        return null;
    }

}
