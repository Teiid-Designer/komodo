/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.core.ShowPropertiesCommand;

/**
 * A shell command to show the complete VDB state.
 */
public final class ShowVdbCommand extends VdbShellCommand {

    static final String NAME = "show-vdb"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowVdbCommand( final WorkspaceStatus status ) {
        super( NAME, false, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        getCommand(ShowPropertiesCommand.NAME).execute();
        
        getCommand(ShowDataRolesCommand.NAME).execute();
        getCommand(ShowEntriesCommand.NAME).execute();
        getCommand(ShowModelsCommand.NAME).execute();
        getCommand(ShowImportsCommand.NAME).execute();
        getCommand(ShowTranslatorsCommand.NAME).execute();

        return true;
    }

}
