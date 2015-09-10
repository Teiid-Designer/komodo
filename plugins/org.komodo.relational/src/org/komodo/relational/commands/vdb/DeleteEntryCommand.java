/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.vdb.VdbCommandMessages.DeleteEntryCommand.ENTRY_DELETED;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.MISSING_ENTRY_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to delete an entry from a VDB.
 */
public final class DeleteEntryCommand extends VdbShellCommand {

    static final String NAME = "delete-entry"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteEntryCommand( final WorkspaceStatus status ) {
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

        final Vdb vdb = getVdb();
        vdb.removeEntry( getTransaction(), entryName );

        // Print success message
        print(MESSAGE_INDENT, getMessage(ENTRY_DELETED,entryName));
        
        return true;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        final UnitOfWork uow = getTransaction();
        final Vdb vdb = getVdb();
        final Entry[] entries = vdb.getEntries( uow );
        List<String> existingEntryNames = new ArrayList<String>(entries.length);
        for(Entry entry : entries) {
            existingEntryNames.add(entry.getName(uow));
        }
        
        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingEntryNames );
            } else {
                for ( final String item : existingEntryNames ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            }

            return 0;
        }

        // no tab completion
        return -1;
    }

}
