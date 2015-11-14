/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.vdb.VdbCommandMessages.AddEntryCommand.ENTRY_ADDED;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.MISSING_ENTRY_NAME;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.MISSING_ENTRY_PATH;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add an entry to a VDB.
 */
public final class AddEntryCommand extends VdbShellCommand {

    static final String NAME = "add-entry"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddEntryCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        CommandResult result = null;

        try {
            final String entryName = requiredArgument( 0, getMessage( MISSING_ENTRY_NAME ) );
            final String entryPath = requiredArgument( 1, getMessage( MISSING_ENTRY_PATH ) );

            final Vdb vdb = getVdb();
            vdb.addEntry( getTransaction(), entryName, entryPath );

            result = new CommandResultImpl( getMessage( ENTRY_ADDED, entryName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( e );
        }

        return result;
    }

    @Override
    public boolean isEnabled() {
        return false;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 2;
    }

}
