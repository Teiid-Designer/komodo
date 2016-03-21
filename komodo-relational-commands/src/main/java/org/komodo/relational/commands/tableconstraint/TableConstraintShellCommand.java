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

import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.TableConstraint;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * A base class for @{link {@link Schema Schema}-related shell commands.
 */
abstract class TableConstraintShellCommand extends RelationalShellCommand {

    protected TableConstraintShellCommand( final WorkspaceStatus status,
                                           final String name ) {
        super( status, name );
    }

    protected final TableConstraint getTableConstraint() throws Exception {
        final KomodoObject kobject = getContext();
        assert( kobject instanceof TableConstraint );
        return ( TableConstraint )kobject;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        final KomodoObject kobject = getContext();
        final Repository.UnitOfWork uow = getTransaction();

        try {
            return AccessPattern.RESOLVER.resolvable( uow, kobject )
                   || ForeignKey.RESOLVER.resolvable( uow, kobject )
                   || Index.RESOLVER.resolvable( uow, kobject )
                   || PrimaryKey.RESOLVER.resolvable( uow, kobject )
                   || UniqueConstraint.RESOLVER.resolvable( uow, kobject );
        } catch ( final Exception e ) {
            return false;
        }
    }

}
