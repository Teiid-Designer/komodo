/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.entry;

import org.komodo.relational.vdb.Entry;
import org.komodo.shell.DisabledShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.RenameCommand;

/**
 * Overrides default RenameCommand to disable it for Entry, since Entry has no children to rename
 */
public final class RenameChildCommand extends DisabledShellCommand {

    static final String NAME = RenameCommand.NAME; // override

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public RenameChildCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        try {
            return Entry.RESOLVER.resolvable( getTransaction(), getContext() );
        } catch ( final Exception e ) {
            return false;
        }
    }

}
