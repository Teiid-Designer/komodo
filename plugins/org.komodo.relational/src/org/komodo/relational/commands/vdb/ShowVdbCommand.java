/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.vdb.VdbCommandMessages.ShowVdbCommand.SHOW_VDB_ERROR;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.ShowPropertiesCommand;

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
            result = getCommand( ShowPropertiesCommand.NAME ).execute();

            if ( !result.isOk() ) {
                return result;
            }

            result = getCommand( ShowDataRolesCommand.NAME ).execute();

            if ( !result.isOk() ) {
                return result;
            }

            result = getCommand( ShowEntriesCommand.NAME ).execute();

            if ( !result.isOk() ) {
                return result;
            }

            result = getCommand( ShowModelsCommand.NAME ).execute();

            if ( !result.isOk() ) {
                return result;
            }

            result = getCommand( ShowImportsCommand.NAME ).execute();

            if ( !result.isOk() ) {
                return result;
            }

            result = getCommand( ShowTranslatorsCommand.NAME ).execute();

            if ( !result.isOk() ) {
                return result;
            }

            return CommandResult.SUCCESS;
        } catch ( final Exception e ) {
            return new CommandResultImpl( false, getMessage( SHOW_VDB_ERROR ), e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 0;
    }

}
