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
import org.komodo.shell.commands.RemoveDescriptorCommand;

/**
 * A disabled shell command that overrides the default {@link RemoveDescriptorCommand}. The relational workspace does not
 * adding or removing descriptors.
 */
public final class RelationalRemoveDescriptorCommand extends DisabledShellCommand {

    static final String NAME = RemoveDescriptorCommand.NAME; // override

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public RelationalRemoveDescriptorCommand( final WorkspaceStatus status ) {
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
