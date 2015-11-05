/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.server.ServerCommandMessages.ServerDatasourcesCommand.InfoMessage;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.PrintUtils;
import org.komodo.spi.runtime.TeiidDataSource;

/**
 * A shell command to show all vdbs on a server
 */
public final class ServerDatasourcesCommand extends ServerShellCommand {

    static final String NAME = "server-datasources"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerDatasourcesCommand( final WorkspaceStatus status ) {
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
            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            // Print title
            final String title = getMessage( InfoMessage, getWorkspaceServerName() );
            print( MESSAGE_INDENT, title );

            Teiid teiid = getWorkspaceServer();
            List< String > objNames = new ArrayList< String >();
            Collection< TeiidDataSource > sources = teiid.getTeiidInstance( getTransaction() ).getDataSources();
            for ( TeiidDataSource source : sources ) {
                String name = source.getDisplayName();
                objNames.add( name );
            }
            Collections.sort(objNames);
            PrintUtils.printMultiLineItemList( MESSAGE_INDENT, getWriter(), objNames, 4, null );
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
