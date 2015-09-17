/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.table;

import static org.komodo.relational.commands.table.TableCommandMessages.AddForeignKeyCommand.FOREIGN_KEY_ADDED;
import static org.komodo.relational.commands.table.TableCommandMessages.General.MISSING_FOREIGN_KEY_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a Foreign Key to a Table.
 */
public final class AddForeignKeyCommand extends TableShellCommand {

    static final String NAME = "add-foreign-key"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddForeignKeyCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String fkName = requiredArgument( 0, getMessage(MISSING_FOREIGN_KEY_NAME) );

        //final Table table = getTable();
        //table.addForeignKey( getTransaction(), fkName );
        
        // Print success message
        print(MESSAGE_INDENT, getMessage(FOREIGN_KEY_ADDED,fkName));
        
        return true;
    }

}
