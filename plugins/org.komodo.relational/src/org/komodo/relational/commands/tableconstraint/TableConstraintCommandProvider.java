/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.tableconstraint;

import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.model.TableConstraint;
import org.komodo.relational.model.internal.AccessPatternImpl;
import org.komodo.relational.model.internal.ForeignKeyImpl;
import org.komodo.relational.model.internal.IndexImpl;
import org.komodo.relational.model.internal.PrimaryKeyImpl;
import org.komodo.relational.model.internal.UniqueConstraintImpl;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command provider for {@link TableConstraint}s.
 */
public class TableConstraintCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider for {@link TableConstraint} shell commands.
     */
    public TableConstraintCommandProvider() {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#getStatusMessage(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public String getStatusMessage( final UnitOfWork uow,
                                    final KomodoObject kObj ) {
        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#getTypeDisplay(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public String getTypeDisplay( final UnitOfWork uow,
                                  final KomodoObject kObject ) throws KException {
        final TableConstraint resolved = resolve( uow, kObject );
        return ( ( resolved == null ) ? null : resolved.getTypeDisplayName() );
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
    public Map< String, Class< ? extends ShellCommand > > provideCommands() {
        final Map< String, Class< ? extends ShellCommand > > result = new HashMap< >();

        result.put( AddConstraintColumnCommand.NAME, AddConstraintColumnCommand.class );
        result.put( DeleteConstraintColumnCommand.NAME, DeleteConstraintColumnCommand.class );

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
    public TableConstraint resolve( final UnitOfWork uow,
                                    final KomodoObject kObject ) throws KException {
        if ( PrimaryKeyImpl.RESOLVER.resolvable( uow, kObject ) ) {
            return PrimaryKeyImpl.RESOLVER.resolve( uow, kObject );
        }

        if ( ForeignKeyImpl.RESOLVER.resolvable( uow, kObject ) ) {
            return ForeignKeyImpl.RESOLVER.resolve( uow, kObject );
        }

        if ( IndexImpl.RESOLVER.resolvable( uow, kObject ) ) {
            return IndexImpl.RESOLVER.resolve( uow, kObject );
        }

        if ( AccessPatternImpl.RESOLVER.resolvable( uow, kObject ) ) {
            return AccessPatternImpl.RESOLVER.resolve( uow, kObject );
        }

        if ( UniqueConstraintImpl.RESOLVER.resolvable( uow, kObject ) ) {
            return UniqueConstraintImpl.RESOLVER.resolve( uow, kObject );
        }

        return null;
    }

}
