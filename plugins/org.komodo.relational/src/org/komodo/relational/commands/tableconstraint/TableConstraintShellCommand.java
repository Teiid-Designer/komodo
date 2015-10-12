/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.tableconstraint;

import org.komodo.relational.Messages;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.TableConstraint;
import org.komodo.relational.model.internal.AccessPatternImpl;
import org.komodo.relational.model.internal.ForeignKeyImpl;
import org.komodo.relational.model.internal.IndexImpl;
import org.komodo.relational.model.internal.PrimaryKeyImpl;
import org.komodo.relational.model.internal.UniqueConstraintImpl;
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.RelationalShellCommand#getMessage(java.lang.Enum, java.lang.Object[])
     */
    @Override
    protected String getMessage( final Enum< ? > key,
                                 final Object... parameters ) {
        return Messages.getString( TableConstraintCommandMessages.RESOURCE_BUNDLE, key.toString(), parameters );
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
            return AccessPatternImpl.RESOLVER.resolvable( uow, kobject )
                   || ForeignKeyImpl.RESOLVER.resolvable( uow, kobject )
                   || IndexImpl.RESOLVER.resolvable( uow, kobject )
                   || PrimaryKeyImpl.RESOLVER.resolvable( uow, kobject )
                   || UniqueConstraintImpl.RESOLVER.resolvable( uow, kobject );
        } catch ( final Exception e ) {
            return false;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, Messages.getString( TableConstraintCommandMessages.RESOURCE_BUNDLE,
                                           getClass().getSimpleName() + ".help", //$NON-NLS-1$
                                           getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent,
               Messages.getString( TableConstraintCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".examples" ) ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent,
               Messages.getString( TableConstraintCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

}
