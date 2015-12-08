/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Model.Type;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * A shell command to deploy a workspace VDB to the connected server.
 */
public final class ServerDeployVdbCommand extends ServerShellCommand {

    static final String NAME = "server-deploy-vdb"; //$NON-NLS-1$

    private static final List< String > VALID_OVERWRITE_ARGS = Arrays.asList( new String[] { "-o", "--overwrite" } ); //$NON-NLS-1$ //$NON-NLS-2$;
    private static final String VDB_DEPLOYMENT_SUFFIX = "-vdb.xml"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerDeployVdbCommand( final WorkspaceStatus status ) {
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
            String vdbName = requiredArgument( 0, I18n.bind( ServerCommandsI18n.missingVdbName ) );
            final String overwriteArg = optionalArgument( 1, null );
            final boolean overwrite = !StringUtils.isBlank( overwriteArg );

            // make sure overwrite arg is valid
            if ( overwrite && !VALID_OVERWRITE_ARGS.contains( overwriteArg ) ) {
                return new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.overwriteArgInvalid, overwriteArg ), null );
            }

            // Return if VDB object not found
            if(!getWorkspaceManager().hasChild(getTransaction(), vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE)) {
                return new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.workspaceVdbNotFound, vdbName ), null );
            }

            // Find the VDB to deploy
            final KomodoObject vdbObj = getWorkspaceManager().getChild(getTransaction(), vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE);
            final Vdb vdbToDeploy = Vdb.RESOLVER.resolve(getTransaction(), vdbObj);

            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            // Deploy the selected VDB
            TeiidInstance teiidInstance = getWorkspaceTeiidInstance();

            // Determine if the server already has a deployed VDB with this name and version
            boolean serverHasVdb = serverHasVdb( teiidInstance,
                                                 vdbToDeploy.getName( getTransaction() ),
                                                 vdbToDeploy.getVersion( getTransaction() ) );
            if(serverHasVdb && !overwrite) {
                return new CommandResultImpl( false,
                                              I18n.bind( ServerCommandsI18n.vdbDeploymentOverwriteDisabled,
                                                         vdbName,
                                                         vdbToDeploy.getVersion( getTransaction() ) ),
                                              null );
            }

            // All VDB source model jndis must exist on the connected server
            Set<String> sourceJndiNames = getPhysicalModelJndis(vdbToDeploy);
            if(!sourceJndiNames.isEmpty()) {
                List<String> serverJndiNames = ServerUtils.getDatasourceJndiNames(teiidInstance);
                for(String sourceJndiName : sourceJndiNames) {
                    if(!serverJndiNames.contains(sourceJndiName)) {
                        return new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.vdbDeployFailedMissingSourceJndi, sourceJndiName ), null);
                    }
                }
            }
            
            // Get VDB content
            String vdbXml = vdbToDeploy.export(getTransaction(), null);
            if (vdbXml == null || vdbXml.isEmpty()) {
                return new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.vdbExportFailed ), null);
            }

            String vdbToDeployName = vdbToDeploy.getName(getTransaction());
            String vdbDeploymentName = vdbToDeployName + VDB_DEPLOYMENT_SUFFIX;
            InputStream stream = new ByteArrayInputStream(vdbXml.getBytes());
            teiidInstance.deployDynamicVdb(vdbDeploymentName, stream);

            print( MESSAGE_INDENT, I18n.bind(ServerCommandsI18n.vdbDeployFinished) );
            result = CommandResult.SUCCESS;
        } catch ( final Exception e ) {
            result = new CommandResultImpl( e );
        }

        return result;
    }
    
    /*
     * Gets the set of unique source jndi names used by the VDB
     */
    private Set<String> getPhysicalModelJndis(Vdb theVdb) throws Exception {
        // The set of Physical Modl Jndis
        HashSet<String> physicalModelJndis = new HashSet<String>();
        
        Model[] models = theVdb.getModels(getTransaction());
        for(Model model : models) {
            Model.Type modelType = model.getModelType(getTransaction());
            if(modelType == Type.PHYSICAL) {
                ModelSource[] sources = model.getSources(getTransaction());
                for(ModelSource source : sources) {
                    String sourceJndiName = source.getJndiName(getTransaction());
                    if(!StringUtils.isEmpty(sourceJndiName)) {
                        physicalModelJndis.add(sourceJndiName);
                    }
                }
            }
        }
        
        return physicalModelJndis;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverDeployVdbHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverDeployVdbExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverDeployVdbUsage ) );
    }

    private boolean serverHasVdb(TeiidInstance teiidInstance, String vdbName, int vdbVersion) throws Exception {
        // If no VDB with this name, return false;
        if(!teiidInstance.hasVdb(vdbName)) {
            return false;
        }

        // May be multiple versions deployed - see if there is one matching supplied version
        Collection<TeiidVdb> serverVdbs = teiidInstance.getVdbs();
        for(TeiidVdb serverVdb : serverVdbs) {
            if(serverVdb.getName().equals(vdbName) && serverVdb.getVersion()==vdbVersion) {
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        final UnitOfWork uow = getTransaction();
        final WorkspaceManager mgr = getWorkspaceManager();
        final KomodoObject[] vdbs = mgr.findVdbs(uow);
        List<String> existingVdbNames = new ArrayList<String>(vdbs.length);
        for(KomodoObject vdb : vdbs) {
            existingVdbNames.add(vdb.getName(uow));
        }
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
        return TabCompletionModifier.AUTO;
    }

}
