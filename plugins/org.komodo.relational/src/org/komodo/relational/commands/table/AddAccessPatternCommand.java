/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.table;

import static org.komodo.relational.commands.table.TableCommandMessages.AddAccessPatternCommand.ACCESS_PATTERN_ADDED;
import static org.komodo.relational.commands.table.TableCommandMessages.AddAccessPatternCommand.ADD_ACCESS_PATTERN_ERROR;
import static org.komodo.relational.commands.table.TableCommandMessages.General.MISSING_ACCESS_PATTERN_NAME;
import org.komodo.relational.model.Table;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
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
            final String apName = requiredArgument( 0, getMessage( MISSING_ACCESS_PATTERN_NAME ) );

            final Table table = getTable();
            table.addAccessPattern( getTransaction(), apName );

            result = new CommandResultImpl( getMessage( ACCESS_PATTERN_ADDED, apName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( ADD_ACCESS_PATTERN_ERROR ), e );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 1;
    }

}
