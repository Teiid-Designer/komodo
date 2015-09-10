/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.vdb;

import static org.komodo.relational.commands.vdb.VdbCommandMessages.DeleteTranslatorCommand.TRANSLATOR_DELETED;
import static org.komodo.relational.commands.vdb.VdbCommandMessages.General.MISSING_TRANSLATOR_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to delete a translator from a VDB.
 */
public class DeleteTranslatorCommand extends VdbShellCommand {

    static final String NAME = "delete-translator"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteTranslatorCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#execute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String translatorName = requiredArgument( 0, getMessage(MISSING_TRANSLATOR_NAME) );

        final Vdb vdb = getVdb();
        vdb.removeTranslator( getTransaction(), translatorName );

        // Print success message
        print(MESSAGE_INDENT, getMessage(TRANSLATOR_DELETED,translatorName));
        
        return true;
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

        final UnitOfWork uow = getTransaction();
        final Vdb vdb = getVdb();
        final Translator[] translators = vdb.getTranslators( uow );
        List<String> existingTranslatorNames = new ArrayList<String>(translators.length);
        for(Translator translator : translators) {
            existingTranslatorNames.add(translator.getName(uow));
        }
        
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
