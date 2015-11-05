/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.server.ServerCommandMessages.Common.MissingTranslatorName;
import static org.komodo.relational.commands.server.ServerCommandMessages.Common.ServerTranslatorNotFound;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerTranslatorCommand.InfoMessage;
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
import org.komodo.spi.runtime.TeiidTranslator;

/**
 * A shell command to show details for a server translator.
 */
public final class ServerTranslatorCommand extends ServerShellCommand {

    static final String NAME = "server-translator"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerTranslatorCommand( final WorkspaceStatus status ) {
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
            final String translatorName = requiredArgument( 0, getMessage( MissingTranslatorName ) );
            
            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            Teiid teiid = getWorkspaceServer();
            TeiidTranslator translator = teiid.getTeiidInstance( getTransaction() ).getTranslator(translatorName);
            if(translator==null) {
                return new CommandResultImpl(false, getMessage( ServerTranslatorNotFound, translatorName ), null);
            }
            
            // Print title
            final String title = getMessage( InfoMessage, translatorName, getWorkspaceServerName() );
            print( MESSAGE_INDENT, title );

            // Print Translator Info
            ServerObjPrintUtils.printTranslatorDetails(getWriter(), MESSAGE_INDENT, translator);
            
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
        List< String > existingTranslatorNames = new ArrayList< String >();
        Collection< TeiidTranslator > translators = teiid.getTeiidInstance( getTransaction() ).getTranslators();
        for ( TeiidTranslator translator : translators ) {
            String name = translator.getName();
            existingTranslatorNames.add( name );
        }
        Collections.sort(existingTranslatorNames);

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingTranslatorNames );
            } else {
                for ( final String item : existingTranslatorNames ) {
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
