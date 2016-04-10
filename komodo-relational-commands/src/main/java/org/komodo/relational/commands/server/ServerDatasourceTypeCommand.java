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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to show details for a server data source type.
 */
public final class ServerDatasourceTypeCommand extends ServerShellCommand {

    static final String NAME = "server-datasource-type"; //$NON-NLS-1$
    
    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerDatasourceTypeCommand( final WorkspaceStatus status ) {
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
            final String sourceTypeName = requiredArgument( 0, I18n.bind( ServerCommandsI18n.missingDatasourceTypeName ) );

            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            Collection<TeiidPropertyDefinition> propDefns = null;
            try {
                // Check the data source type names to make sure its valid
                Set< String > typeNames = getWorkspaceTeiidInstance().getDataSourceTypeNames();
                if(!typeNames.contains(sourceTypeName)) {
                    return new CommandResultImpl(false, I18n.bind( ServerCommandsI18n.serverDatasourceTypeNotFound, sourceTypeName ), null);
                }
                // Get the source type properties
                propDefns = getWorkspaceTeiidInstance().getTemplatePropertyDefns(sourceTypeName);
            } catch (Exception ex) {
                result = new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.connectionErrorWillDisconnect ), ex );
                WkspStatusServerManager.getInstance(getWorkspaceStatus()).disconnectDefaultServer();
                return result;
            }
            if(propDefns==null) {
                return new CommandResultImpl( false,
                                              I18n.bind( ServerCommandsI18n.serverDatasourceTypeNotFound, sourceTypeName ),
                                              null );
            }

            // Print title
            final String title = I18n.bind( ServerCommandsI18n.infoMessageDatasourceType,
                                            sourceTypeName,
                                            getWorkspaceServerName() );
            print( MESSAGE_INDENT, title );
            print( MESSAGE_INDENT, I18n.bind( ServerCommandsI18n.datasourceTypePropertiesHeader ));

            // Print DataSource Template Info
            ServerObjPrintUtils.printDatasourceTemplateProperties(getWriter(), MESSAGE_INDENT, propDefns, 
                                                                  I18n.bind( ServerCommandsI18n.datasourceTypeNameLabel ), 
                                                                  I18n.bind( ServerCommandsI18n.datasourceTypeDefaultValueLabel ) );

            result = CommandResult.SUCCESS;
        } catch ( final Exception e ) {
            result = new CommandResultImpl( e );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.RelationalShellCommand#get()
     */
    @Override
    protected RelationalObject get() throws Exception {
        return super.get();
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
        print( indent, I18n.bind( ServerCommandsI18n.serverDatasourceTypeHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverDatasourceTypeExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverDatasourceTypeUsage ) );
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
            Teiid teiid = getWorkspaceServer();
            Set< String > types = teiid.getTeiidInstance( getTransaction() ).getDataSourceTypeNames();
            List< String > existingTypes = new ArrayList< String >(types);
            Collections.sort(existingTypes);

            if ( args.isEmpty() ) {
                if ( lastArgument == null ) {
                    candidates.addAll( existingTypes );
                } else {
                    for ( final String item : existingTypes ) {
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
