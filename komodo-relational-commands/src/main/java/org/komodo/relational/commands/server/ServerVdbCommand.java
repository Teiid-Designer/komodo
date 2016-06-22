/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational.commands.server;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Collections;
import java.util.List;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to show details for a server VDB
 */
public final class ServerVdbCommand extends ServerShellCommand {

    static final String NAME = "server-vdb"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerVdbCommand( final WorkspaceStatus status ) {
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
            final String vdbName = requiredArgument( 0, I18n.bind( ServerCommandsI18n.missingVdbName ) );

            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            TeiidVdb vdb = null; 
            try {
                // Check the vdb name to make sure its valid
                List< String > existingVdbNames = ServerUtils.getVdbNames(getWorkspaceTeiidInstance());
                if(!existingVdbNames.contains(vdbName)) {
                    return new CommandResultImpl(false, I18n.bind( ServerCommandsI18n.serverVdbNotFound, vdbName ), null);
                }
                // Get the vdb
                vdb = getWorkspaceTeiidInstance().getVdb(vdbName);
            } catch (Exception ex) {
                result = new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.serverGetVdbError, vdbName, ex.getLocalizedMessage() ), null );
                return result;
            }
            if(vdb==null) {
                return new CommandResultImpl(false, I18n.bind( ServerCommandsI18n.serverVdbNotFound, vdbName ), null);
            }

            // Print title
            final String title = I18n.bind( ServerCommandsI18n.infoMessageVdb, vdbName, getWorkspaceServerName() );
            print( MESSAGE_INDENT, title );

            // Print VDB Info
            ServerObjPrintUtils.printVdbDetails(getWriter(), MESSAGE_INDENT, vdb);

            result = CommandResult.SUCCESS;
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
        return 1;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverVdbHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverVdbExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverVdbUsage ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        try {
            List< String > existingVdbNames = ServerUtils.getVdbNames( getWorkspaceTeiidInstance() );
            Collections.sort(existingVdbNames);

            if ( args.isEmpty() ) {
                if ( lastArgument == null ) {
                    candidates.addAll( existingVdbNames );
                } else {
                    for ( final String item : existingVdbNames ) {
                        if ( item.startsWith( lastArgument ) ) {
                            candidates.add( item );
                        }
                    }
                }
            }
        } catch (Exception ex) {
            print( );
            print( MESSAGE_INDENT, I18n.bind(ServerCommandsI18n.connectionErrorWillDisconnect) );
            WkspStatusServerManager.getInstance(getWorkspaceStatus()).disconnectDefaultServer();
        }
        return TabCompletionModifier.AUTO;
    }

}
