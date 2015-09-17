/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.permission;

import static org.komodo.relational.commands.permission.PermissionCommandMessages.AddConditionCommand.CONDITION_ADDED;
import static org.komodo.relational.commands.permission.PermissionCommandMessages.General.MISSING_CONDITION_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.vdb.Permission;
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
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String permissionName = requiredArgument( 0, getMessage(MISSING_CONDITION_NAME) );

        final Permission permission = getPermission();
        permission.addCondition( getTransaction(), permissionName );

        // Print success message
        print(MESSAGE_INDENT, getMessage(CONDITION_ADDED,permissionName));
        
        return true;
    }

}
