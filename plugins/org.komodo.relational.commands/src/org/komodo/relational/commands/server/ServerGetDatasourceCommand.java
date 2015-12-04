/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to get a server Datasource and copy into the workspace
 */
public final class ServerGetDatasourceCommand extends ServerShellCommand {

    static final String NAME = "server-get-datasource"; //$NON-NLS-1$

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

            // Make sure no datasource currently in workspace with this name
            final WorkspaceManager mgr = getWorkspaceManager();
            KomodoObject[] repoDS = mgr.getChildrenOfType(getTransaction(), KomodoLexicon.DataSource.NODE_TYPE, datasourceName);
            if(repoDS.length!=0) {
                return new CommandResultImpl( false, I18n.bind(ServerCommandsI18n.repoDatasourceWithNameExists, datasourceName), null );
            }
            
            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            // Get the teiid instance
            TeiidInstance teiidInstance = getWorkspaceServer().getTeiidInstance(getTransaction());

            // Get the Data Source from the server
            TeiidDataSource dataSource = teiidInstance.getDataSource(datasourceName);
            if(dataSource == null) {
                return new CommandResultImpl( false, I18n.bind(ServerCommandsI18n.serverDatasourceNotFound, datasourceName), null );
            }

            Properties serverDsProps = dataSource.getProperties();
            // TODO: look at other methods - do we need to add setters/getters.
            
            // Create the Data Source and set properties
            Datasource newDatasource = mgr.createDatasource( getTransaction(), null, datasourceName );
            setRepoDatasourceProperties(newDatasource, serverDsProps);
            
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
        return 1;
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

    private boolean isWorkspaceContext() {
        try {
            final KomodoType contextType = getContext().getTypeIdentifier( getTransaction() );
            return ( contextType == KomodoType.WORKSPACE );
        } catch ( final Exception e ) {
            return false;
        }
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

        List<String> existingDatasourceNames = ServerUtils.getDatasourceNames(getWorkspaceServer(),getTransaction());
        Collections.sort(existingDatasourceNames);

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingDatasourceNames );
            } else {
                for ( final String item : existingDatasourceNames ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            }
        }

        return TabCompletionModifier.AUTO;
    }
    
    /*
     * Transfer the server Datasource properties to the repo object
     */
    private void setRepoDatasourceProperties(Datasource repoSource, Properties serverDsProperties) throws Exception {
        for(String key : serverDsProperties.stringPropertyNames()) {
            String value = serverDsProperties.getProperty(key);
            if(key.equals(SERVER_DS_PROP_JNDINAME)) { 
                repoSource.setJndiName(getTransaction(), value);
            } else if(key.equals(SERVER_DS_PROP_DRIVERNAME)) { 
                repoSource.setDriverName(getTransaction(), value);
            } else if(key.equals(SERVER_DS_PROP_CLASSNAME)) { 
                repoSource.setClassName(getTransaction(), value);
                repoSource.setJdbc(getTransaction(), false);
            } else {
                repoSource.setProperty(getTransaction(), key, value);
            }
        }
    }

}
