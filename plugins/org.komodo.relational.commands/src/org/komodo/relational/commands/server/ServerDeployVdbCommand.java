/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.server.ServerCommandMessages.Common.MissingVdbName;
import static org.komodo.relational.commands.server.ServerCommandMessages.Common.WorkspaceVdbNotFound;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerDeployVdbCommand.VdbDeployFinished;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerDeployVdbCommand.VdbExportFailed;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.TeiidInstance;

/**
 * A shell command to deploy a workspace VDB to the connected server.
 */
public final class ServerDeployVdbCommand extends ServerShellCommand {

    static final String NAME = "server-deploy-vdb"; //$NON-NLS-1$

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
            String vdbName = requiredArgument( 0, getMessage( MissingVdbName ) );
            
            // Find the VDB to deploy
            final WorkspaceManager mgr = getWorkspaceManager();
            final Vdb[] vdbs = mgr.findVdbs(getTransaction());
            Vdb vdbToDeploy = null;
            for(Vdb vdb : vdbs) {
                if(vdb.getName(getTransaction()).equals(vdbName)) {
                    vdbToDeploy = vdb;
                    break;
                }
            }
            
            // Return if VDB object not found
            if(vdbToDeploy==null) {
                return new CommandResultImpl( false, getMessage( WorkspaceVdbNotFound ), null );
            }
            
            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            // Deploy the selected VDB
            Teiid teiid = getWorkspaceServer();
            TeiidInstance teiidInstance = teiid.getTeiidInstance(getTransaction());
            
            String vdbXml = vdbToDeploy.export(getTransaction(), null);
            if (vdbXml == null || vdbXml.isEmpty()) {
                return new CommandResultImpl( false, getMessage( VdbExportFailed ), null);
            }

            String vdbToDeployName = vdbToDeploy.getName(getTransaction());
            String vdbDeploymentName = vdbToDeployName + VDB_DEPLOYMENT_SUFFIX;
            InputStream stream = new ByteArrayInputStream(vdbXml.getBytes());
            teiidInstance.deployDynamicVdb(vdbDeploymentName, stream);
            
            print( MESSAGE_INDENT, getMessage(VdbDeployFinished) );
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
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        final UnitOfWork uow = getTransaction();
        final WorkspaceManager mgr = getWorkspaceManager();
        final KomodoObject[] vdbs = mgr.findVdbs(uow);
        List<String> existingVdbNames = new ArrayList<String>(vdbs.length);
        for(KomodoObject vdb : vdbs) {
            existingVdbNames.add(vdb.getName(uow));
        }

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingVdbNames );
            } else {
                for ( final String item : existingVdbNames ) {
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
    
}
