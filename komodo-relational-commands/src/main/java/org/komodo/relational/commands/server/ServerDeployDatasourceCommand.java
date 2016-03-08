/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.datasource.Datasource;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

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
            if(!getWorkspaceManager().hasChild(getTransaction(), sourceName, KomodoLexicon.DataSource.NODE_TYPE)) {
                return new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.workspaceDatasourceNotFound, sourceName ), null );
            }

            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            final TeiidInstance teiidInstance = getWorkspaceTeiidInstance();
            final KomodoObject datasourceObj = getWorkspaceManager().getChild(getTransaction(), sourceName, KomodoLexicon.DataSource.NODE_TYPE);
            final Datasource sourceToDeploy = Datasource.RESOLVER.resolve(getTransaction(), datasourceObj);

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
                Properties sourceProps = sourceToDeploy.getPropertiesForServerDeployment(getTransaction(), teiidInstance);
                
                // If overwriting, delete the existing source first
                if(serverHasDatasource) {
                    teiidInstance.deleteDataSource(sourceName);
                }
                // Create the source
                teiidInstance.getOrCreateDataSource(sourceName, sourceName, sourceType, sourceProps);
            } catch (Exception ex) {
                result = new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.connectionErrorWillDisconnect ), ex );
                ServerManager.getInstance(getWorkspaceStatus()).disconnectDefaultServer();
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

        final KomodoObject[] datasources = getWorkspaceManager().findDatasources(getTransaction());
        List<String> existingDatasourceNames = new ArrayList<String>(datasources.length);
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
