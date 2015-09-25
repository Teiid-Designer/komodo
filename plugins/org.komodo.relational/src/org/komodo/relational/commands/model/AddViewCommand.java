/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.model.ModelCommandMessages.AddViewCommand.ADD_VIEW_ERROR;
import static org.komodo.relational.commands.model.ModelCommandMessages.AddViewCommand.VIEW_ADDED;
import static org.komodo.relational.commands.model.ModelCommandMessages.General.MISSING_VIEW_NAME;
import org.komodo.relational.model.Model;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * A shell command to add a View to a Model.
 */
public final class AddViewCommand extends ModelShellCommand {

    static final String NAME = "add-view"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddViewCommand( final WorkspaceStatus status ) {
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
            final String viewName = requiredArgument( 0, getMessage( MISSING_VIEW_NAME ) );

            final Model model = getModel();
            model.addView( getTransaction(), viewName );

            result = new CommandResultImpl( getMessage( VIEW_ADDED, viewName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( ADD_VIEW_ERROR ), e );
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
