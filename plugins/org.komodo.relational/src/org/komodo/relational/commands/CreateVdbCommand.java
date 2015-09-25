/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import static org.komodo.relational.commands.WorkspaceCommandMessages.CreateVdbCommand.CREATE_VDB_ERROR;
import static org.komodo.relational.commands.WorkspaceCommandMessages.CreateVdbCommand.MISSING_VDB_EXTERNAL_PATH;
import static org.komodo.relational.commands.WorkspaceCommandMessages.CreateVdbCommand.MISSING_VDB_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.CreateVdbCommand.VDB_CREATED;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoType;

/**
 * A shell command to create a VDB.
 */
public final class CreateVdbCommand extends RelationalShellCommand {

    static final String NAME = "create-vdb"; //$NON-NLS-1$

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
            final String extPath = requiredArgument( 1, getMessage( MISSING_VDB_EXTERNAL_PATH ) );

            final WorkspaceManager mgr = getWorkspaceManager();
            mgr.createVdb( getTransaction(), null, vdbName, extPath );

            result = new CommandResultImpl( getMessage( VDB_CREATED, vdbName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( CREATE_VDB_ERROR ), e );
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        // Only allow VDB create in the workspace
        try {
            KomodoType contextType = getContext().getTypeIdentifier(getTransaction());
            return contextType==KomodoType.WORKSPACE;
        } catch (Exception ex) {
            // on exception will return false
        }
        return false;
    }

}
