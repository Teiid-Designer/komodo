/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import static org.komodo.relational.commands.WorkspaceCommandMessages.CreateSchemaCommand.CREATE_SCHEMA_ERROR;
import static org.komodo.relational.commands.WorkspaceCommandMessages.CreateSchemaCommand.MISSING_SCHEMA_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.CreateSchemaCommand.SCHEMA_CREATED;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoType;

/**
 * A shell command to create a Schema.
 */
public final class CreateSchemaCommand extends RelationalShellCommand {

    static final String NAME = "create-schema"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public CreateSchemaCommand( final WorkspaceStatus status ) {
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
            final String schemaName = requiredArgument( 0, getMessage( MISSING_SCHEMA_NAME ) );

            final WorkspaceManager mgr = getWorkspaceManager();
            mgr.createSchema( getTransaction(), null, schemaName );

            result = new CommandResultImpl( getMessage( SCHEMA_CREATED, schemaName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( CREATE_SCHEMA_ERROR ), e );
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        // Only allow Schema create in the workspace
        try {
            KomodoType contextType = getContext().getTypeIdentifier( getTransaction() );
            return contextType == KomodoType.WORKSPACE;
        } catch ( Exception ex ) {
            // on exception will return false
        }
        return false;
    }

}
