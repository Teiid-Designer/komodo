/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.NAME_TYPE_DISPLAY;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.ShowEntriesCommand.ENTRIES_HEADER;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.ShowEntriesCommand.NO_ENTRIES;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to show all entries in a VDB.
 */
public final class ShowEntriesCommand extends VdbShellCommand {

    static final String NAME = "show-entries"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowEntriesCommand( final WorkspaceStatus status ) {
        super( NAME, false, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final UnitOfWork uow = getTransaction();
        final Vdb vdb = getVdb();
        final Entry[] entries = vdb.getEntries( uow );

        if ( entries.length == 0 ) {
            print( MESSAGE_INDENT, getMessage(NO_ENTRIES, vdb.getName(uow)) );
        } else {
            print( MESSAGE_INDENT, getMessage(ENTRIES_HEADER, vdb.getName(uow)) );
            List<String> names = new ArrayList<String>(entries.length);
            for ( final Entry entry : entries ) {
                print(MESSAGE_INDENT,getMessage(NAME_TYPE_DISPLAY,entry.getName(uow),entry.getTypeDisplayName()));
            }
        }
        print();

        return true;
    }

}
