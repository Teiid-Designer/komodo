/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.model.ModelCommandMessages.DeleteSourceCommand.SOURCE_DELETED;
import static org.komodo.relational.commands.model.ModelCommandMessages.General.MISSING_SOURCE_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.ModelSource;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to delete a Source from a Model.
 */
public final class DeleteSourceCommand extends ModelShellCommand {

    static final String NAME = "delete-source"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteSourceCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String sourceName = requiredArgument( 0, getMessage(MISSING_SOURCE_NAME) );

        final Model model = getModel();
        model.removeSource( getTransaction(), sourceName );

        // Print success message
        print(MESSAGE_INDENT, getMessage(SOURCE_DELETED,sourceName));
        
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
        final Model model = getModel();
        final ModelSource[] sources = model.getSources( uow );
        List<String> existingSourceNames = new ArrayList<String>(sources.length);
        for(ModelSource source : sources) {
            existingSourceNames.add(source.getName(uow));
        }
        
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
