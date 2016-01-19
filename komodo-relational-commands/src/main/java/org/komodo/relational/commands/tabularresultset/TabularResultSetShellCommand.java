/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.tabularresultset;

import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link TabularResultSet TabularResultSet}-related shell commands.
 */
abstract class TabularResultSetShellCommand extends RelationalShellCommand {

    protected TabularResultSetShellCommand( final String name,
                                           final WorkspaceStatus status ) {
        super( status, name );
    }

    protected TabularResultSet getTabularResultSet() throws Exception {
        assert getContext() instanceof TabularResultSet;
        return TabularResultSet.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return TabularResultSet.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            // exception returns false
        }
        return false;
    }

}
