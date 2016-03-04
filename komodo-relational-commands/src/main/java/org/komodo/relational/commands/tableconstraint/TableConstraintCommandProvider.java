/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
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
