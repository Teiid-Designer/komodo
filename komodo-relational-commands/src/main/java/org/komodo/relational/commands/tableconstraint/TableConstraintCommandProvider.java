/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.tableconstraint;

import java.util.HashSet;
import java.util.Set;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.TableConstraint;
import org.komodo.relational.model.UniqueConstraint;
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
        final Set< Class< ? extends ShellCommand > > result = new HashSet< >();

        result.add( AddConstraintColumnCommand.class );
        result.add( DeleteConstraintColumnCommand.class );

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
        if ( PrimaryKey.RESOLVER.resolvable( uow, kObject ) ) {
            return PrimaryKey.RESOLVER.resolve( uow, kObject );
        }

        if ( ForeignKey.RESOLVER.resolvable( uow, kObject ) ) {
            return ForeignKey.RESOLVER.resolve( uow, kObject );
        }

        if ( Index.RESOLVER.resolvable( uow, kObject ) ) {
            return Index.RESOLVER.resolve( uow, kObject );
        }

        if ( AccessPattern.RESOLVER.resolvable( uow, kObject ) ) {
            return AccessPattern.RESOLVER.resolve( uow, kObject );
        }

        if ( UniqueConstraint.RESOLVER.resolvable( uow, kObject ) ) {
            return UniqueConstraint.RESOLVER.resolve( uow, kObject );
        }

        return null;
    }

}
