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
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import org.komodo.relational.connection.Connection;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * A shell command to deploy a workspace Datasource to the connected server.
 */
public final class ServerDeployDatasourceCommand extends ServerShellCommand {

    static final String NAME = "server-deploy-datasource"; //$NON-NLS-1$

    private static final List< String > VALID_OVERWRITE_ARGS = Arrays.asList( new String[] { "-o", "--overwrite" } ); //$NON-NLS-1$ //$NON-NLS-2$;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerDeployDatasourceCommand( final WorkspaceStatus status ) {
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
            String sourceName = requiredArgument( 0, I18n.bind( ServerCommandsI18n.missingDatasourceName ) );
            final String overwriteArg = optionalArgument( 1, null );
            final boolean overwrite = !StringUtils.isBlank( overwriteArg );

            // make sure overwrite arg is valid
            if ( overwrite && !VALID_OVERWRITE_ARGS.contains( overwriteArg ) ) {
                return new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.overwriteArgInvalid, overwriteArg ), null );
            }

            // Make sure datasource object exists in repo
            if(!getWorkspaceManager(getTransaction()).hasChild(getTransaction(), sourceName, DataVirtLexicon.Connection.NODE_TYPE)) {
                return new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.workspaceDatasourceNotFound, sourceName ), null );
            }

            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            final TeiidInstance teiidInstance = getWorkspaceTeiidInstance();
            final KomodoObject datasourceObj = getWorkspaceManager(getTransaction()).getChild( getTransaction(),
                                                                               sourceName,
                                                                               DataVirtLexicon.Connection.NODE_TYPE );

            final Connection sourceToDeploy = Connection.RESOLVER.resolve(getTransaction(), datasourceObj);

            // Make sure that the sourceType is OK for the connected server.
            String sourceType = sourceToDeploy.getDriverName(getTransaction());
            if(StringUtils.isEmpty(sourceType) || !ServerUtils.hasDatasourceType(teiidInstance, sourceType)) {
                return new CommandResultImpl( false,
                                              I18n.bind( ServerCommandsI18n.datasourceDeploymentTypeNotFound,
                                                         sourceType ),
                                              null );
            }

            // Determine if the server already has a deployed Datasource with this name
            try {
                boolean serverHasDatasource = teiidInstance.dataSourceExists(sourceName);
                if(serverHasDatasource && !overwrite) {
                    return new CommandResultImpl( false,
                                                  I18n.bind( ServerCommandsI18n.datasourceDeploymentOverwriteDisabled,
                                                             sourceName ),
                                                  null );
                }

                // Get the properties necessary for deployment to server
                Properties sourceProps = null;
                try {
                    sourceToDeploy.getPropertiesForServerDeployment(getTransaction(), teiidInstance);
                } catch (Exception ex) {
                    result = new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.datasourcePropertiesError, ex.getLocalizedMessage() ), null );
                    return result;
                }

                try {
                    // If overwriting, delete the existing source first
                    if(serverHasDatasource) {
                        teiidInstance.deleteDataSource(sourceName);
                    }
                    // Create the source
                    teiidInstance.getOrCreateDataSource(sourceName, sourceName, sourceType, sourceProps);
                } catch (Exception ex) {
                    result = new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.datasourceDeploymentError, ex.getLocalizedMessage() ), null );
                    return result;
                }
            } catch (Exception ex) {
                result = new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.connectionErrorWillDisconnect ), ex );
                WkspStatusServerManager.getInstance(getWorkspaceStatus()).disconnectDefaultServer();
                return result;
            }

            print( MESSAGE_INDENT, I18n.bind(ServerCommandsI18n.datasourceDeployFinished) );
            result = CommandResult.SUCCESS;
        } catch ( final Exception e ) {
            result = new CommandResultImpl( e );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverDeployDatasourceHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverDeployDatasourceExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverDeployDatasourceUsage ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 2;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        return (isWorkspaceContext() && hasConnectedWorkspaceServer());
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

        final KomodoObject[] datasources = getWorkspaceManager(getTransaction()).findConnections(getTransaction());
        List<String> existingDatasourceNames = new ArrayList<>(datasources.length);
        for(KomodoObject datasource : datasources) {
            existingDatasourceNames.add(datasource.getName(getTransaction()));
        }
        Collections.sort(existingDatasourceNames);

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingDatasourceNames );
            } else {
                for ( final String item : existingDatasourceNames ) {
                    if ( item.startsWith( lastArgument ) ) {
                        candidates.add( item );
                    }
                }
            }
        }

        return TabCompletionModifier.AUTO;
    }

}
