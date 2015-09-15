/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model.table;

import static org.komodo.relational.commands.model.table.TableCommandMessages.AddUniqueConstraintCommand.UNIQUE_CONSTRAINT_ADDED;
import static org.komodo.relational.commands.model.table.TableCommandMessages.General.MISSING_UNIQUE_CONSTRAINT_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.model.Table;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a Unique Constraint to a Table.
 */
public final class AddUniqueConstraintCommand extends TableShellCommand {

    static final String NAME = "add-unique-constraint"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddUniqueConstraintCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String ucName = requiredArgument( 0, getMessage(MISSING_UNIQUE_CONSTRAINT_NAME) );

        final Table table = getTable();
        table.addUniqueConstraint( getTransaction(), ucName );
        
        // Print success message
        print(MESSAGE_INDENT, getMessage(UNIQUE_CONSTRAINT_ADDED,ucName));
        
        return true;
    }

}
