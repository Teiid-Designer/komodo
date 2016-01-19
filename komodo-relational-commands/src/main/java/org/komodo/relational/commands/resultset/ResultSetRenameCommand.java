/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.resultset;

import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.shell.DisabledShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.RenameCommand;

/**
 * Overrides the default {@link RenameCommand} to disable it for all result sets.
 */
public final class ResultSetRenameCommand extends DisabledShellCommand {

    static final String NAME = RenameCommand.NAME; // override

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ResultSetRenameCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        try {
            return ( DataTypeResultSet.RESOLVER.resolvable( getTransaction(), getContext() )
                     || TabularResultSet.RESOLVER.resolvable( getTransaction(), getContext() ) );
        } catch ( final Exception e ) {
            return false;
        }
    }

}
