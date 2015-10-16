/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.server.ServerCommandMessages.ServerShowVdbsCommand.InfoMessage;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerShowVdbsCommand.ListHeader;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
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
            // Validates that a server is connected (prints output for errors)
            boolean hasConnectedDefault = validateHasConnectedWorkspaceServer();
            if ( !hasConnectedDefault ) {
                return new CommandResultImpl( false, null, null );
            }

            // Print title
            final String title = getMessage( InfoMessage, getWorkspaceServerName() );
            print( MESSAGE_INDENT, title );

            Teiid teiid = getWorkspaceServer();
            List< String > objNames = new ArrayList< String >();
            Collection< TeiidVdb > vdbs = teiid.getTeiidInstance( getTransaction() ).getVdbs();
            for ( TeiidVdb vdb : vdbs ) {
                String name = vdb.getName();
                objNames.add( name );
            }
            PrintUtils.printList( getWriter(), objNames, getMessage( ListHeader ) );
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
        return 0;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        return hasConnectedWorkspaceServer();
    }
}
