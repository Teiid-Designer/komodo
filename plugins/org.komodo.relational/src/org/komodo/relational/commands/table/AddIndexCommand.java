/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.table;

import static org.komodo.relational.commands.table.TableCommandMessages.AddIndexCommand.INDEX_ADDED;
import static org.komodo.relational.commands.table.TableCommandMessages.General.MISSING_INDEX_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.model.Table;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add an Index to a Table.
 */
public final class AddIndexCommand extends TableShellCommand {

    static final String NAME = "add-index"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddIndexCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String indexName = requiredArgument( 0, getMessage(MISSING_INDEX_NAME) );

        final Table table = getTable();
        table.addIndex( getTransaction(), indexName );
        
        // Print success message
        print(MESSAGE_INDENT, getMessage(INDEX_ADDED,indexName));
        
        return true;
    }

}
