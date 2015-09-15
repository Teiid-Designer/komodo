/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import static org.komodo.relational.commands.WorkspaceCommandMessages.CreateTeiidCommand.MISSING_TEIID_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.CreateTeiidCommand.TEIID_CREATED;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoType;

/**
 * A shell command to create a Teiid object.
 */
public final class CreateTeiidCommand extends RelationalShellCommand {

    static final String NAME = "create-teiid"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public CreateTeiidCommand( final WorkspaceStatus status ) {
        super( status, true, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String teiidName = requiredArgument( 0, getMessage(MISSING_TEIID_NAME) );

        final WorkspaceManager mgr = getWorkspaceManager();
        mgr.createTeiid( getTransaction(), null, teiidName );

        // Print success message
        print(MESSAGE_INDENT, getMessage(TEIID_CREATED,teiidName));
        
        return true;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        boolean isValid = false;
        
        // Only allow Teiid create in the workspace
        try {
            KomodoType contextType = getWorkspaceStatus().getCurrentContext().getTypeIdentifier(getTransaction());
            isValid = (contextType==KomodoType.WORKSPACE);
        } catch (Exception ex) {
            // on exception will return false
        }
        return isValid;
    }

}
