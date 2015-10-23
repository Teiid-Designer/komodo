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
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.runtime.TeiidInstance;

/**
 * A base class for @{link {@link Vdb VDB}-related shell commands.
 */
abstract class ServerShellCommand extends RelationalShellCommand {

    protected ServerShellCommand( final String name,
                                  final WorkspaceStatus status ) {
        super( status, name );
    }

    /**
     * Validates the existence of a connected server.
     * @return the result
     * @throws KException the exception
     */
    protected CommandResult validateHasConnectedWorkspaceServer() throws KException {
        if(!hasWorkspaceServer()) {
            return new CommandResultImpl(false, getMessage(NoTeiidDefined), null );
        }

        Teiid teiid = getWorkspaceServer();
        if(!isConnected(teiid)) {
            return new CommandResultImpl(false, getMessage(ServerNotConnected), null );
        }
        return CommandResult.SUCCESS;
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
        if(kObj!=null && Teiid.RESOLVER.resolvable(getWorkspaceStatus().getTransaction(), kObj)) {
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
            print( MESSAGE_INDENT, ex.getLocalizedMessage() );
        }

        return isConnected(teiid);
    }

    @Override
    protected String getMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(ServerCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }

    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, Messages.getString( ServerCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".help", getName() ) ); //$NON-NLS-1$
    }

    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, Messages.getString( ServerCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".examples" ) ); //$NON-NLS-1$
    }

    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, Messages.getString( ServerCommandMessages.RESOURCE_BUNDLE, getClass().getSimpleName() + ".usage" ) ); //$NON-NLS-1$
    }

}
