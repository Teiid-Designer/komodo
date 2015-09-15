/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.relational.commands.model.ModelCommandMessages.DeleteUserDefinedFunctionCommand.USER_DEFINED_FUNCTION_DELETED;
import static org.komodo.relational.commands.model.ModelCommandMessages.General.MISSING_USER_DEFINED_FUNCTION_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Model;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to delete a User Defined Function from a Model.
 */
public final class DeleteUserDefinedFunctionCommand extends ModelShellCommand {

    static final String NAME = "delete-user-defined-function"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteUserDefinedFunctionCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String functionName = requiredArgument( 0, getMessage(MISSING_USER_DEFINED_FUNCTION_NAME) );

        final Model model = getModel();
        model.removeFunction( getTransaction(), functionName );

        // Print success message
        print(MESSAGE_INDENT, getMessage(USER_DEFINED_FUNCTION_DELETED,functionName));
        
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
        final Function[] functions = model.getFunctions( uow );
        List<String> existingFunctionNames = new ArrayList<String>(functions.length);
        for(Function function : functions) {
            existingFunctionNames.add(function.getName(uow));
        }
        
        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingFunctionNames );
            } else {
                for ( final String item : existingFunctionNames ) {
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
