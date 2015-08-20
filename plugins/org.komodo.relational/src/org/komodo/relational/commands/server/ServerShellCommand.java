/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import java.util.List;
import org.komodo.relational.Messages;
import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.utils.StringUtils;

/**
 * A base class for @{link {@link Vdb VDB}-related shell commands.
 */
abstract class ServerShellCommand extends RelationalShellCommand {

    protected ServerShellCommand( final String name,
                               final boolean shouldCommit,
                               final WorkspaceStatus status ) {
        super( name, shouldCommit, status );
    }

    protected boolean hasDefaultServer() {
        String server = getWorkspaceStatus().getServer();
        if(StringUtils.isBlank(server)) {
            return false;
        }
        return true;
    }
    
    protected String getDefaultServerName() {
        return getWorkspaceStatus().getServer();
    }
    
    protected Teiid getDefaultServer() throws Exception {
        String server = getWorkspaceStatus().getServer();
        if(StringUtils.isBlank(server)) {
            return null;
        }
        
        List<Teiid> teiids = getWorkspaceManager().findTeiids(getTransaction());

        if (teiids == null || teiids.size() == 0) {
            return null;
        }

        Teiid teiid = null;
        for (Teiid theTeiid : teiids) {
            String teiidName = theTeiid.getName(getTransaction());
            if (server.equals(theTeiid.getId(getTransaction())) || server.equals(teiidName)) {
                teiid = theTeiid;
                break;
            }
        }
        return teiid;
    }
    
    protected boolean isConnected( final Teiid teiid ) {
        if (teiid == null) {
            return false;
        }

        TeiidInstance teiidInstance = teiid.getTeiidInstance(getTransaction());
        return teiidInstance.isConnected();
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        return isCurrentTypeValid( Vdb.TYPE_ID );
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
