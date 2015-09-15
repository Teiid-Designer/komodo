/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.server.ServerCommandMessages.Common.NoTeiidDefined;
import static org.komodo.relational.commands.server.ServerCommandMessages.Common.ServerNotConnected;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.relational.Messages;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.teiid.internal.TeiidImpl;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.runtime.TeiidInstance;

/**
 * A base class for @{link {@link Vdb VDB}-related shell commands.
 */
abstract class ServerShellCommand extends RelationalShellCommand {

    protected ServerShellCommand( final String name,
                               final boolean shouldCommit,
                               final WorkspaceStatus status ) {
        super( status, shouldCommit, name );
    }

    /** 
     * Validates the existence of a connected server, and prints output
     * depending on the problem
     * @return 'true' if there is a default connected server
     * @throws KException the exception
     */
    protected boolean validateHasConnectedWorkspaceServer() throws KException {
        if(!hasWorkspaceServer()) {
            print(CompletionConstants.MESSAGE_INDENT, getMessage(NoTeiidDefined));
            return false;
        }
        
        Teiid teiid = getWorkspaceServer();
        if(!isConnected(teiid)) {
            print( MESSAGE_INDENT, getMessage( ServerNotConnected ) ); 
            return false;
        }
        return true;
    }
    
    protected boolean hasWorkspaceServer() {
        KomodoObject kObj = null;
        try {
            kObj = getWorkspaceServer();
        } catch (KException ex) {
            // exception just return false
        }
        return kObj!=null;
    }
    
    protected String getWorkspaceServerName() throws KException {
        KomodoObject server = getWorkspaceServer();
        if(server!=null) {
            return server.getName(getWorkspaceStatus().getTransaction());
        }
        return null;
    }
    
    protected Teiid getWorkspaceServer() throws KException {
        KomodoObject kObj = getWorkspaceStatus().getStateObjects().get(ServerCommandProvider.SERVER_DEFAULT_KEY);
        if(kObj!=null && TeiidImpl.RESOLVER.resolvable(getWorkspaceStatus().getTransaction(), kObj)) {
            return (Teiid)kObj;
        }
        return null;
    }
    
    protected boolean isConnected( final Teiid teiid ) {
        if (teiid == null) {
            return false;
        }

        TeiidInstance teiidInstance = teiid.getTeiidInstance(getTransaction());
        return teiidInstance.isConnected();
    }

    protected boolean hasConnectedWorkspaceServer( ) {
        Teiid teiid = null;
        try {
            teiid = getWorkspaceServer();
        } catch (Exception ex) {
        }
        
        return isConnected(teiid);
    }
    
    @Override
    protected String getMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(ServerCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }
    
    /**
     * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
     */
    @Override
    public void printHelp( final int indent ) {
        print( indent, Messages.getString( ServerCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".help" ) ); //$NON-NLS-1$
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
     */
    @Override
    public void printUsage( final int indent ) {
        print( indent, Messages.getString( ServerCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

}
