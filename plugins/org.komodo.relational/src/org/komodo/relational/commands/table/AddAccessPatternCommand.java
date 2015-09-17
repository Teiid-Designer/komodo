/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.table;

import static org.komodo.relational.commands.table.TableCommandMessages.AddAccessPatternCommand.ACCESS_PATTERN_ADDED;
import static org.komodo.relational.commands.table.TableCommandMessages.General.MISSING_ACCESS_PATTERN_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.model.Table;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add an AccessPattern to a Table.
 */
public final class AddAccessPatternCommand extends TableShellCommand {

    static final String NAME = "add-access-pattern"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddAccessPatternCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String apName = requiredArgument( 0, getMessage(MISSING_ACCESS_PATTERN_NAME) );

        final Table table = getTable();
        table.addAccessPattern( getTransaction(), apName );
        
        // Print success message
        print(MESSAGE_INDENT, getMessage(ACCESS_PATTERN_ADDED,apName));
        
        return true;
    }

}
