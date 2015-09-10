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
import java.util.List;
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
    protected boolean validateHasConnectedDefaultServer() throws KException {
        if(!hasDefaultServer()) {
            print(CompletionConstants.MESSAGE_INDENT, getMessage(NoTeiidDefined));
            return false;
        }
        
        Teiid teiid = getDefaultServer();
        if(!isConnected(teiid)) {
            print( MESSAGE_INDENT, getMessage( ServerNotConnected ) ); 
            return false;
        }
        return true;
    }
    
    protected boolean hasDefaultServer() {
        KomodoObject server = null;
        try {
            server = getWorkspaceStatus().getServer();
        } catch (KException ex) {
            // on exception returns null object
        }
        if(server!=null) return true;
        return false;
    }
    
    protected String getDefaultServerName() throws KException {
        KomodoObject server = null;
        try {
            server = getWorkspaceStatus().getServer();
        } catch (KException ex) {
            // on exception returns null object
        }
        if(server!=null) {
            return server.getName(getWorkspaceStatus().getTransaction());
        }
        return null;
    }
    
    protected Teiid getDefaultServer() throws KException {
        KomodoObject kObj = getWorkspaceStatus().getServer();
        if(TeiidImpl.RESOLVER.resolvable(getWorkspaceStatus().getTransaction(), kObj)) {
            return (Teiid)kObj;
        }
        return null;
    }
    
    /**
     * Get the teiid object with the supplied name from the workspace
     * @param serverName the name of the teiid object
     * @return the teiid object
     * @throws Exception the exception
     */
    protected Teiid getWorkspaceTeiid(String serverName) throws Exception {
        Teiid resultTeiid = null;
        List<Teiid> teiids = getWorkspaceManager().findTeiids(getTransaction());

        if (teiids == null || teiids.size() == 0) {
            return resultTeiid;
        }

        for (Teiid theTeiid : teiids) {
            String teiidName = theTeiid.getName(getTransaction());
            if (serverName.equals(theTeiid.getId(getTransaction())) || serverName.equals(teiidName)) {
                resultTeiid = theTeiid;
                break;
            }
        }
        return resultTeiid;
    }
    
    protected boolean isConnected( final Teiid teiid ) {
        if (teiid == null) {
            return false;
        }

        TeiidInstance teiidInstance = teiid.getTeiidInstance(getTransaction());
        return teiidInstance.isConnected();
    }

    protected boolean hasConnectedDefaultTeiid( ) {
        Teiid teiid = null;
        try {
            teiid = getDefaultServer();
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
