/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.MISSING_ENTRY_NAME;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.MISSING_ENTRY_PATH;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.AddEntryCommand.ENTRY_ADDED;
import org.komodo.relational.vdb.Vdb;
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
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String entryName = requiredArgument( 0, getMessage(MISSING_ENTRY_NAME) );
        final String entryPath = requiredArgument( 1, getMessage(MISSING_ENTRY_PATH) );

        final Vdb vdb = getVdb();
        vdb.addEntry( getTransaction(), entryName, entryPath );

        // Print success message
        print(MESSAGE_INDENT, getMessage(ENTRY_ADDED,entryName));
        
        return true;
    }

}
