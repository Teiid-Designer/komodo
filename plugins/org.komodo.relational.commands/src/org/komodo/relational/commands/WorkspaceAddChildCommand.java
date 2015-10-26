/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.AddChildCommand;

/**
 * A disabled shell command that overrides the default {@link AddChildCommand}. The relatinoal workspace does not
 * allow custom children.
 */
public final class WorkspaceAddChildCommand extends WorkspaceShellCommand {

    static final String NAME = AddChildCommand.NAME; // override

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public WorkspaceAddChildCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        assert false;
        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 0;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#isEnabled()
     */
    @Override
    public boolean isEnabled() {
        return false;
    }

}
