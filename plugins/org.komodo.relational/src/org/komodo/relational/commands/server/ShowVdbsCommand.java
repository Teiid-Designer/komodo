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
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.PrintUtils;
import org.komodo.spi.runtime.TeiidVdb;

/**
 * A shell command to show all vdbs on a server
 */
public final class ShowVdbsCommand extends ServerShellCommand {

    static final String NAME = "show-vdbs"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowVdbsCommand( final WorkspaceStatus status ) {
        super( NAME, false, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        if(hasDefaultServer()) {
            Teiid teiid = getDefaultServer();
            
            if(isConnected(teiid)) {
                // Print title
                final String title = Messages.getString( Messages.ServerCommand.ServerTypeHeader, getDefaultServerName(), "VDB" ); 
                print( MESSAGE_INDENT, title );
                
                List<String> objNames = new ArrayList<String>();
                Collection<TeiidVdb> vdbs = teiid.getTeiidInstance(getTransaction()).getVdbs();
                for(TeiidVdb vdb : vdbs) {
                    String name = vdb.getName();
                    objNames.add(name);
                }
                PrintUtils.printList(this, objNames, Messages.getString( Messages.ServerCommand.ObjectNameHeader ));
                print();
            }
        }

        return true;
    }
    
}
