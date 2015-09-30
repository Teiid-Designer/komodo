/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import static org.komodo.relational.commands.WorkspaceCommandMessages.CreateTeiidCommand.CREATE_TEIID_ERROR;
import static org.komodo.relational.commands.WorkspaceCommandMessages.CreateTeiidCommand.MISSING_TEIID_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.CreateTeiidCommand.TEIID_CREATED;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to create a Teiid object.
 */
public final class CreateTeiidCommand extends WorkspaceShellCommand {

    static final String NAME = "create-teiid"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public CreateTeiidCommand( final WorkspaceStatus status ) {
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
            final String teiidName = requiredArgument( 0, getMessage( MISSING_TEIID_NAME ) );

            final WorkspaceManager mgr = getWorkspaceManager();
            mgr.createTeiid( getTransaction(), null, teiidName );

            result = new CommandResultImpl( getMessage( TEIID_CREATED, teiidName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( CREATE_TEIID_ERROR ), e );
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
