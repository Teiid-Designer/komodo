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
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
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

            // Return if workspace Datasource object not found
            if(!getWorkspaceManager().hasChild(getTransaction(), sourceName, KomodoLexicon.DataSource.NODE_TYPE)) {
                return new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.workspaceDatasourceNotFound, sourceName ), null );
            }

            // Get the Data Source to deploy
            final KomodoObject datasourceObj = getWorkspaceManager().getChild(getTransaction(), sourceName, KomodoLexicon.DataSource.NODE_TYPE);
            final Datasource sourceToDeploy = Datasource.RESOLVER.resolve(getTransaction(), datasourceObj);

            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            // Deploy the selected Data Source
            Teiid teiid = getWorkspaceServer();
            TeiidInstance teiidInstance = teiid.getTeiidInstance(getTransaction());

            // Determine if the server already has a deployed Datasource with this name
            String sourceToDeployName = sourceToDeploy.getName(getTransaction());
            boolean serverHasDatasource = teiidInstance.dataSourceExists(sourceToDeployName);
            if(serverHasDatasource && !overwrite) {
                return new CommandResultImpl( false,
                                              I18n.bind( ServerCommandsI18n.datasourceDeploymentOverwriteDisabled,
                                                         sourceName ),
                                              null );
            }
            
            // Validate that the sourceType is OK for the connected server.
            String sourceType = sourceToDeploy.getDriverName(getTransaction());
            boolean serverHasDatasourceType = serverHasDatasourceType(teiidInstance, sourceType);
            if(!serverHasDatasourceType) {
                return new CommandResultImpl( false,
                                              I18n.bind( ServerCommandsI18n.datasourceDeploymentTypeNotFound,
                                                         sourceType ),
                                              null );
            }

            // Get the remaining source properties from the repo object necessary for deployment to server
            Properties sourceProps = getPropertiesForServerDeployment(teiidInstance, sourceToDeploy);
            
            // If overwriting, delete the existing source first
            if(serverHasDatasource) {
                teiidInstance.deleteDataSource(sourceToDeployName);
            }
            // Create the source
            teiidInstance.getOrCreateDataSource(sourceToDeployName, sourceToDeployName, sourceType, sourceProps);

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

    // Assemble the repo source properties to supply for server deployment
    private Properties getPropertiesForServerDeployment(TeiidInstance teiidInstance, Datasource sourceToDeploy) throws Exception {
        Properties sourceProps = new Properties();
        
        // Get the Property Defns for this type of source.
        Collection<TeiidPropertyDefinition> templatePropDefns = teiidInstance.getTemplatePropertyDefns(sourceToDeploy.getDriverName(getTransaction()));
        Collection<String> templatePropNames = getTemplatePropNames(templatePropDefns);

        if(sourceToDeploy.isJdbc(getTransaction())) {
            String driverName = sourceToDeploy.getDriverName(getTransaction());
            sourceProps.setProperty(SERVER_DS_PROP_DRIVERNAME,driverName);
            String jndiName = sourceToDeploy.getJndiName(getTransaction());
            sourceProps.setProperty(SERVER_DS_PROP_JNDINAME, jndiName);
        } else {
            sourceProps.setProperty(SERVER_DS_PROP_CLASSNAME, sourceToDeploy.getClassName(getTransaction()));
        }
        
        // Iterate the supplied datasource properties.  Compare them against the valid properties for the server source type.
        String[] propNames = sourceToDeploy.getPropertyNames(getTransaction());
        for(String propName : propNames) {
            if(templatePropNames.contains(propName)) {
                TeiidPropertyDefinition propDefn = getTemplatePropertyDefn(templatePropDefns,propName);
                boolean hasDefault = propDefn.getDefaultValue()!=null ? true : false;
                String sourcePropValue = sourceToDeploy.getProperty(getTransaction(), propName).getStringValue(getTransaction());
                // Template has no default - set the property
                if(!hasDefault) {
                    sourceProps.setProperty(propName, sourcePropValue);
                // Template has default - if source property matches it, no need to provide it.
                } else {
                    String templateDefaultValue = propDefn.getDefaultValue().toString();
                    if(!templateDefaultValue.equals(sourcePropValue)) {
                        sourceProps.setProperty(propName, sourcePropValue);
                    }
                }
            }
        }
        
        return sourceProps;
    }
    
    private Collection<String> getTemplatePropNames(Collection<TeiidPropertyDefinition> templatePropDefns) {
        Collection<String> propNames = new ArrayList<String>();
        for(TeiidPropertyDefinition propDefn : templatePropDefns) {
            propNames.add(propDefn.getName());
        }
        return propNames;
    }

    private TeiidPropertyDefinition getTemplatePropertyDefn(Collection<TeiidPropertyDefinition> templatePropDefns, String propName) {
        TeiidPropertyDefinition propDefn = null;
        for(TeiidPropertyDefinition aDefn : templatePropDefns) {
            if(propName.equals(aDefn.getName())) {
                propDefn = aDefn;
                break;
            }
        }
        return propDefn;
    }
    
    private boolean serverHasDatasourceType(TeiidInstance teiidInstance, String sourceType) throws Exception {
        // Look for matching name
        Set<String> serverTypes = teiidInstance.getDataSourceTypeNames();
        for(String serverType : serverTypes) {
            if(serverType.equals(sourceType)) {
                return true;
            }
        }

        return false;
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
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            }
        }

        return TabCompletionModifier.AUTO;
    }

}
