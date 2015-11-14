/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.server.ServerCommandMessages.Common.MissingDatasourceTypeName;
import static org.komodo.relational.commands.server.ServerCommandMessages.Common.ServerDatasourceTypeNotFound;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerDatasourceTypeCommand.InfoMessage;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.runtime.TeiidPropertyDefinition;

/**
 * A shell command to show details for a server data source type.
 */
public final class ServerDatasourceTypeCommand extends ServerShellCommand {

    static final String NAME = "server-datasource-type"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerDatasourceTypeCommand( final WorkspaceStatus status ) {
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
            final String sourceTypeName = requiredArgument( 0, getMessage( MissingDatasourceTypeName ) );
            
            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            Teiid teiid = getWorkspaceServer();
            Collection<TeiidPropertyDefinition> propDefns = teiid.getTeiidInstance( getTransaction() ).getTemplatePropertyDefns(sourceTypeName);
            if(propDefns==null) {
                return new CommandResultImpl(false, getMessage( ServerDatasourceTypeNotFound, sourceTypeName ), null);
            }
            
            // Print title
            final String title = getMessage( InfoMessage, sourceTypeName, getWorkspaceServerName() );
            print( MESSAGE_INDENT, title );
            print( MESSAGE_INDENT, "DataSource Template Properties:" ); //$NON-NLS-1$

            // Print DataSource Template Info
            ServerObjPrintUtils.printDatasourceTemplateProperties(getWriter(), MESSAGE_INDENT, propDefns, "Name", "Default Value"); //$NON-NLS-1$  //$NON-NLS-2$

            result = CommandResult.SUCCESS;
        } catch ( final Exception e ) {
            result = new CommandResultImpl( e );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.RelationalShellCommand#get()
     */
    @Override
    protected RelationalObject get() throws Exception {
        return super.get();
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
        List< String > existingTypes = new ArrayList< String >();
        Set< String > types = teiid.getTeiidInstance( getTransaction() ).getDataSourceTypeNames();
        for ( String type : types ) {
            existingTypes.add( type );
        }
        Collections.sort(existingTypes);

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingTypes );
            } else {
                for ( final String item : existingTypes ) {
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
