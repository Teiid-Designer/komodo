/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.server.ServerCommandMessages.Common.MissingVdbName;
import static org.komodo.relational.commands.server.ServerCommandMessages.Common.ServerVdbNotFound;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerVdbCommand.InfoMessage;
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
import org.komodo.spi.runtime.TeiidVdb;

/**
 * A shell command to show details of a server vdb
 */
public final class ServerVdbCommand extends ServerShellCommand {

    static final String NAME = "server-vdb"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerVdbCommand( final WorkspaceStatus status ) {
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
            final String vdbName = requiredArgument( 0, getMessage( MissingVdbName ) );
            
            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            Teiid teiid = getWorkspaceServer();
            TeiidVdb vdb = teiid.getTeiidInstance( getTransaction() ).getVdb(vdbName);
            if(vdb==null) {
                return new CommandResultImpl(false, getMessage( ServerVdbNotFound, vdbName ), null);
            }
            
            // Print title
            final String title = getMessage( InfoMessage, vdbName, getWorkspaceServerName() );
            print( MESSAGE_INDENT, title );

            // Print VDB Info
            ServerObjPrintUtils.printVdbDetails(getWriter(), MESSAGE_INDENT, vdb);

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
        List< String > existingVdbNames = new ArrayList< String >();
        Collection< TeiidVdb > vdbs = teiid.getTeiidInstance( getTransaction() ).getVdbs();
        for ( TeiidVdb vdb : vdbs ) {
            String name = vdb.getName();
            existingVdbNames.add( name );
        }
        Collections.sort(existingVdbNames);

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingVdbNames );
            } else {
                for ( final String item : existingVdbNames ) {
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
