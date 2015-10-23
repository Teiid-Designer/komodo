/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.condition;

import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.commands.condition.RenameChildCommand;
import org.komodo.relational.vdb.Condition;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A shell command provider for Condition.
 */
public class ConditionCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for Condition shell commands.
     */
    public ConditionCommandProvider() {
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

        result.put( SetConditionPropertyCommand.NAME, SetConditionPropertyCommand.class );
        result.put( UnsetConditionPropertyCommand.NAME, UnsetConditionPropertyCommand.class );
        result.put( RenameChildCommand.NAME, RenameChildCommand.class );

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
    public Condition resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(Condition.RESOLVER.resolvable(uow, kObj)) {
            return Condition.RESOLVER.resolve(uow, kObj);
        }
        return null;
    }

    @Override
    public String getTypeDisplay ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        final Condition resolved = resolve( uow, kObj );
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
