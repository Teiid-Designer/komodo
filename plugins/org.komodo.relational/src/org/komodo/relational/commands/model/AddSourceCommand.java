/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.model.ModelCommandMessages.AddSourceCommand.ADD_SOURCE_ERROR;
import static org.komodo.relational.commands.model.ModelCommandMessages.AddSourceCommand.SOURCE_ADDED;
import static org.komodo.relational.commands.model.ModelCommandMessages.General.MISSING_SOURCE_NAME;
import org.komodo.relational.model.Model;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a source to a Model.
 */
public final class AddSourceCommand extends ModelShellCommand {

    static final String NAME = "add-source"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddSourceCommand( final WorkspaceStatus status ) {
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
            final String sourceName = requiredArgument( 0, getMessage( MISSING_SOURCE_NAME ) );

            final Model model = getModel();
            model.addSource( getTransaction(), sourceName );

            result = new CommandResultImpl( getMessage( SOURCE_ADDED, sourceName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( ADD_SOURCE_ERROR ), e );
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
