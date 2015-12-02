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
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
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

            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            // Get the teiid instance
            Teiid teiid = getWorkspaceServer();
            TeiidInstance teiidInstance = teiid.getTeiidInstance(getTransaction());

            // Get the Data Source from the server
            TeiidDataSource dataSource = teiidInstance.getDataSource(datasourceName);
            if(dataSource == null) {
                return new CommandResultImpl( false, I18n.bind(ServerCommandsI18n.serverDatasourceNotFound, datasourceName), null );
            }

            Properties serverDsProps = dataSource.getProperties();
            
            // Create the Data Source and set properties
            final WorkspaceManager mgr = getWorkspaceManager();
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
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        Collection<String> existingDatasourceNames = getDeployedDatasources();

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

            return 0;
        }

        // no tab completion
        return -1;
    }
    
    /*
     * Transfer the server Datasource properties to the repo object
     */
    private void setRepoDatasourceProperties(Datasource repoSource, Properties serverDsProperties) throws Exception {
        for(String key : serverDsProperties.stringPropertyNames()) {
            String value = serverDsProperties.getProperty(key);
            if(key.equals("jndi-name")) { //$NON-NLS-1$
                repoSource.setJndiName(getTransaction(), value);
            } else if(key.equals("driver-name")) { //$NON-NLS-1$
                repoSource.setDriverName(getTransaction(), value);
            } else {
                repoSource.setProperty(getTransaction(), key, value);
            }
        }
    }

    /*
     * Return the deployed datasources on the workspace server
     */
    private Collection<String> getDeployedDatasources() throws Exception {
        Teiid teiid = getWorkspaceServer();
        List< String > existingSourceNames = new ArrayList< String >();
        Collection< TeiidDataSource > sources = teiid.getTeiidInstance( getTransaction() ).getDataSources();
        for ( TeiidDataSource source : sources ) {
            String name = source.getName();
            existingSourceNames.add( name );
        }
        return existingSourceNames;
    }

}
