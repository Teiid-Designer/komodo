/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.workspace;

import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.CreateVdbCommand.VDB_CREATED;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.MISSING_VDB_NAME;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to create a VDB.
 */
public final class CreateVdbCommand extends WorkspaceShellCommand {

    static final String NAME = "create-vdb"; //$NON-NLS-1$
    static final String DEFAULT_PATH = "defaultPath"; //$NON-NLS-1$
    
    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public CreateVdbCommand( final WorkspaceStatus status ) {
        super( status, NAME );
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
            final String vdbName = requiredArgument( 0, getMessage( MISSING_VDB_NAME ) );
            final String extPath = optionalArgument( 1, DEFAULT_PATH );

            final WorkspaceManager mgr = getWorkspaceManager();
            mgr.createVdb( getTransaction(), null, vdbName, extPath );

            result = new CommandResultImpl( getMessage( VDB_CREATED, vdbName ) );
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
        return 2;
    }

}
