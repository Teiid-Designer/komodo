/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.server.ServerCommandMessages.ServerShowTranslatorsCommand.InfoMessage;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerShowTranslatorsCommand.ListHeader;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.PrintUtils;
import org.komodo.spi.runtime.TeiidTranslator;

/**
 * A shell command to show all translators on a server
 */
public final class ServerShowTranslatorsCommand extends ServerShellCommand {

    static final String NAME = "server-show-translators"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerShowTranslatorsCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        // Validates that a server is connected (prints output for errors)
        boolean hasConnectedDefault = validateHasConnectedDefaultServer();
        if(!hasConnectedDefault) return false;
        
        // Print title
        final String title = getMessage(InfoMessage, getDefaultServerName() );
        print( MESSAGE_INDENT, title );

        Teiid teiid = getDefaultServer();
        List<String> objNames = new ArrayList<String>();
        Collection<TeiidTranslator> translators = teiid.getTeiidInstance(getTransaction()).getTranslators();
        for(TeiidTranslator translator : translators) {
            String name = translator.getName();
            objNames.add(name);
        }
        
        PrintUtils.printList(getWorkspaceStatus(), objNames, getMessage(ListHeader));
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
        return hasConnectedDefaultTeiid();
    }
}
