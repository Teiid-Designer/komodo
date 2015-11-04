/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.table;

import static org.komodo.relational.commands.table.TableCommandMessages.AddPrimaryKeyCommand.PK_EXISTS_CANT_ADD;
import static org.komodo.relational.commands.table.TableCommandMessages.AddPrimaryKeyCommand.PRIMARY_KEY_ADDED;
import static org.komodo.relational.commands.table.TableCommandMessages.General.MISSING_PRIMARY_KEY_NAME;
import org.komodo.relational.model.Table;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a PrimaryKey to a Table.
 */
public final class AddPrimaryKeyCommand extends TableShellCommand {

    static final String NAME = "add-primary-key"; //$NON-NLS-1$

    /** 
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddPrimaryKeyCommand( final WorkspaceStatus status ) {
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
            final String pkName = requiredArgument( 0, getMessage( MISSING_PRIMARY_KEY_NAME ) );

            // Cannot add a PrimaryKey if the table already has one
            final Table table = getTable();
            if( table.getPrimaryKey(getTransaction()) != null) {
                return new CommandResultImpl( getMessage( PK_EXISTS_CANT_ADD, pkName ) );
            }
            
            table.setPrimaryKey(getTransaction(), pkName);
            result = new CommandResultImpl( getMessage( PRIMARY_KEY_ADDED, pkName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( e );
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
