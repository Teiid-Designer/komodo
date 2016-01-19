/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.ShowPropertiesCommand;
import org.komodo.utils.i18n.I18n;

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

            print();
            result = getCommand( ShowDataRolesCommand.NAME ).execute();

            if ( !result.isOk() ) {
                return result;
            }
//
//            result = getCommand( ShowEntriesCommand.NAME ).execute();
//
//            if ( !result.isOk() ) {
//                return result;
//            }

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
            return new CommandResultImpl( e );
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( VdbCommandsI18n.showVdbHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( VdbCommandsI18n.showVdbExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( VdbCommandsI18n.showVdbUsage ) );
    }

}
