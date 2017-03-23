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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Properties;

import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.relational.connection.Connection;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * A shell command to get a server Datasource and copy into the workspace
 */
public final class ServerGetDatasourceCommand extends ServerShellCommand {

    static final String NAME = "server-get-datasource"; //$NON-NLS-1$

    private static final List< String > VALID_OVERWRITE_ARGS = Arrays.asList( new String[] { "-o", "--overwrite" } ); //$NON-NLS-1$ //$NON-NLS-2$;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerGetDatasourceCommand( final WorkspaceStatus status ) {
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
            String datasourceName = requiredArgument( 0, I18n.bind( ServerCommandsI18n.missingDatasourceName ) );

            final String overwriteArg = optionalArgument( 1, null );
            final boolean overwrite = !StringUtils.isBlank( overwriteArg );
            // make sure overwrite arg is valid
            if ( overwrite && !VALID_OVERWRITE_ARGS.contains( overwriteArg ) ) {
                return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.overwriteArgInvalid, overwriteArg ), null );
            }

            // If datasource with same name is in workspace, make sure we can overwrite
            boolean hasDS = getWorkspaceManager(getTransaction()).hasChild(getTransaction(), datasourceName, DataVirtLexicon.Connection.NODE_TYPE);
            if( hasDS && !overwrite ) {
                return new CommandResultImpl( false, I18n.bind(ServerCommandsI18n.datasourceOverwriteNotEnabled, datasourceName), null );
            }

            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            // Get the Data Source from the server
            TeiidDataSource serverDS = null;
            try {
                // Check the data source name to make sure its valid
                List< String > existingSourceNames = ServerUtils.getDatasourceNames(getWorkspaceTeiidInstance());
                if(!existingSourceNames.contains(datasourceName)) {
                    return new CommandResultImpl(false, I18n.bind( ServerCommandsI18n.serverDatasourceNotFound, datasourceName ), null);
                }
                // Get the data source
                serverDS = getWorkspaceTeiidInstance().getDataSource(datasourceName);
            } catch (Exception ex) {
                result = new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.connectionErrorWillDisconnect ), ex );
                WkspStatusServerManager.getInstance(getWorkspaceStatus()).disconnectDefaultServer();
                return result;
            }
            if(serverDS == null) {
                return new CommandResultImpl( false, I18n.bind(ServerCommandsI18n.serverDatasourceNotFound, datasourceName), null );
            }

            // If overwriting, delete existing first
            if(hasDS) {
                final KomodoObject datasourceToDelete = getWorkspaceManager(getTransaction()).getChild( getTransaction(),
                                                                                        datasourceName,
                                                                                        DataVirtLexicon.Connection.NODE_TYPE );
                getWorkspaceManager(getTransaction()).delete(getTransaction(), datasourceToDelete);
            }
            // Create the Data Source and set properties
            Connection newDatasource = getWorkspaceManager(getTransaction()).createConnection( getTransaction(), null, datasourceName );
            setRepoDatasourceProperties(newDatasource, serverDS.getProperties());

            print( MESSAGE_INDENT, I18n.bind(ServerCommandsI18n.datasourceCopyToRepoFinished) );
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
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverGetDatasourceHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverGetDatasourceExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverGetDatasourceUsage ) );
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
            List<String> existingDatasourceNames = ServerUtils.getDatasourceNames(getWorkspaceTeiidInstance());
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
        } catch (Exception ex) {
            print( );
            print( MESSAGE_INDENT, I18n.bind(ServerCommandsI18n.connectionErrorWillDisconnect) );
            WkspStatusServerManager.getInstance(getWorkspaceStatus()).disconnectDefaultServer();
        }

        return TabCompletionModifier.AUTO;
    }

    /*
     * Transfer the server Datasource properties to the repo object
     */
    private void setRepoDatasourceProperties(Connection repoSource, Properties serverDsProperties) throws Exception {
        for(String key : serverDsProperties.stringPropertyNames()) {
            String value = serverDsProperties.getProperty(key);
            if(key.equals(TeiidInstance.DATASOURCE_JNDINAME)) {
                repoSource.setJndiName(getTransaction(), value);
            } else if(key.equals(TeiidInstance.DATASOURCE_DRIVERNAME)) {
                repoSource.setDriverName(getTransaction(), value);
            } else if(key.equals(TeiidInstance.DATASOURCE_CLASSNAME)) {
                repoSource.setClassName(getTransaction(), value);
                repoSource.setJdbc(getTransaction(), false);
            } else {
                repoSource.setProperty(getTransaction(), key, value);
            }
        }
    }

}
