/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
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
