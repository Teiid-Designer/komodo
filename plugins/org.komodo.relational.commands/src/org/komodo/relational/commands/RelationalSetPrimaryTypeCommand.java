/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import org.komodo.relational.RelationalObject;
import org.komodo.shell.DisabledShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.SetPrimaryTypeCommand;

/**
 * A disabled shell command that overrides the default {@link SetPrimaryTypeCommand}. The relational workspace does not
 * allow setting the primary type.
 */
public final class RelationalSetPrimaryTypeCommand extends DisabledShellCommand {

    static final String NAME = SetPrimaryTypeCommand.NAME; // override

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public RelationalSetPrimaryTypeCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return ( getContext() instanceof RelationalObject );
    }

}
