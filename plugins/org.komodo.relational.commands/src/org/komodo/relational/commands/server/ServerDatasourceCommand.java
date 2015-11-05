/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.server.ServerCommandMessages.Common.MissingDatasourceName;
import static org.komodo.relational.commands.server.ServerCommandMessages.Common.ServerDatasourceNotFound;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerDatasourceCommand.InfoMessage;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.runtime.TeiidDataSource;

/**
 * A shell command to show all vdbs on a server
 */
public final class ServerDatasourceCommand extends ServerShellCommand {

    static final String NAME = "server-datasource"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerDatasourceCommand( final WorkspaceStatus status ) {
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
            final String sourceName = requiredArgument( 0, getMessage( MissingDatasourceName ) );
            
            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            Teiid teiid = getWorkspaceServer();
            TeiidDataSource source = teiid.getTeiidInstance( getTransaction() ).getDataSource(sourceName);
            if(source==null) {
                return new CommandResultImpl(false, getMessage( ServerDatasourceNotFound, sourceName ), null);
            }
            
            // Print title
            final String title = getMessage( InfoMessage, sourceName, getWorkspaceServerName() );
            print( MESSAGE_INDENT, title );

            // Print DataSource Info
            ServerObjPrintUtils.printDatasourceDetails(getWriter(), MESSAGE_INDENT, source);
            
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
        return hasConnectedWorkspaceServer();
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

        Teiid teiid = getWorkspaceServer();
        List< String > existingSourceNames = new ArrayList< String >();
        Collection< TeiidDataSource > sources = teiid.getTeiidInstance( getTransaction() ).getDataSources();
        for ( TeiidDataSource source : sources ) {
            String name = source.getName();
            existingSourceNames.add( name );
        }
        Collections.sort(existingSourceNames);

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingSourceNames );
            } else {
                for ( final String item : existingSourceNames ) {
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
