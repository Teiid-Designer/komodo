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
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerShowVdbsCommand.ObjectNameHeader;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerShowVdbsCommand.ServerTypeHeader;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.PrintUtils;
import org.komodo.spi.runtime.TeiidVdb;

/**
 * A shell command to show all vdbs on a server
 */
public final class ServerShowVdbsCommand extends ServerShellCommand {

    static final String NAME = "server-show-vdbs"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerShowVdbsCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        if(!hasDefaultServer()) {
            print(CompletionConstants.MESSAGE_INDENT, getMessage(NoTeiidDefined));
            return false;
        }
        Teiid teiid = getDefaultServer();
        if(!isConnected(teiid)) {
            print( MESSAGE_INDENT, getMessage( ServerNotConnected ) ); 
            return false;
        }
        
        // Print title
        final String title = getMessage(ServerTypeHeader, getDefaultServerName(), "VDB" ); //$NON-NLS-1$
        print( MESSAGE_INDENT, title );

        List<String> objNames = new ArrayList<String>();
        Collection<TeiidVdb> vdbs = teiid.getTeiidInstance(getTransaction()).getVdbs();
        for(TeiidVdb vdb : vdbs) {
            String name = vdb.getName();
            objNames.add(name);
        }
        PrintUtils.printList(getWorkspaceStatus(), objNames, getMessage(ObjectNameHeader));
        print();

        return true;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        return hasConnectedTeiid();
    }
    
}
