/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.MISSING_DATA_ROLE_NAME;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.AddDataRoleCommand.DATA_ROLE_ADDED;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a data role to a VDB.
 */
public final class AddDataRoleCommand extends VdbShellCommand {

    static final String NAME = "add-data-role"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddDataRoleCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String dataRoleName = requiredArgument( 0, getMessage(MISSING_DATA_ROLE_NAME) );

        final Vdb vdb = getVdb();
        vdb.addDataRole( getTransaction(), dataRoleName );

        // Print success message
        print(MESSAGE_INDENT, getMessage(DATA_ROLE_ADDED,dataRoleName));
        
        return true;
    }

}
