/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.permission;

import static org.komodo.relational.commands.permission.PermissionCommandMessages.AddConditionCommand.ADD_CONDITION_ERROR;
import static org.komodo.relational.commands.permission.PermissionCommandMessages.AddConditionCommand.CONDITION_ADDED;
import static org.komodo.relational.commands.permission.PermissionCommandMessages.General.MISSING_CONDITION_NAME;
import org.komodo.relational.vdb.Permission;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a Condition to a Permission.
 */
public final class AddConditionCommand extends PermissionShellCommand {

    static final String NAME = "add-condition"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddConditionCommand( final WorkspaceStatus status ) {
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
            final String permissionName = requiredArgument( 0, getMessage( MISSING_CONDITION_NAME ) );

            final Permission permission = getPermission();
            permission.addCondition( getTransaction(), permissionName );

            result = new CommandResultImpl( getMessage( CONDITION_ADDED, permissionName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( ADD_CONDITION_ERROR ), e );
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
