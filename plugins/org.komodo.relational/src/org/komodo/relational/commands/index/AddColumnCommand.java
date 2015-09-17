/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.index;

import static org.komodo.relational.commands.index.IndexCommandMessages.AddColumnCommand.COLUMN_ADDED;
import static org.komodo.relational.commands.index.IndexCommandMessages.General.MISSING_COLUMN_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.model.Index;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a Column to an Index.
 */
public final class AddColumnCommand extends IndexShellCommand {

    static final String NAME = "add-column"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddColumnCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String columnName = requiredArgument( 0, getMessage(MISSING_COLUMN_NAME) );

        final Index index = getIndex();
        index.addColumn( getTransaction(), columnName );
        
        // Print success message
        print(MESSAGE_INDENT, getMessage(COLUMN_ADDED,columnName));
        
        return true;
    }

}
