/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.foreignkey;

import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.model.ForeignKey;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A base class for @{link {@link ForeignKey ForeignKey}-related shell commands.
 */
abstract class ForeignKeyShellCommand extends RelationalShellCommand {

    protected ForeignKeyShellCommand( final String name,
                                      final WorkspaceStatus status ) {
        super( status, name );
    }

    protected ForeignKey getForeignKey() throws Exception {
        assert getContext() instanceof ForeignKey;
        return ForeignKey.RESOLVER.resolve(getTransaction(), getContext());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return ForeignKey.RESOLVER.resolvable(getTransaction(), getContext());
        } catch (Exception ex) {
            return false;
        }
    }

}
