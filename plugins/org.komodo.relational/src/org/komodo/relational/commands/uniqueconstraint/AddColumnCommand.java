/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.uniqueconstraint;

import static org.komodo.relational.commands.uniqueconstraint.UniqueConstraintCommandMessages.AddColumnCommand.COLUMN_ADDED;
import static org.komodo.relational.commands.uniqueconstraint.UniqueConstraintCommandMessages.General.MISSING_COLUMN_NAME;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.utils.StringUtils;

/**
 * A shell command to add a Column to a UniqueConstraint.
 */
public final class AddColumnCommand extends UniqueConstraintShellCommand {

    static final String NAME = "add-column"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddColumnCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String columnName = requiredArgument( 0, getMessage(MISSING_COLUMN_NAME) );

        final UniqueConstraint uc = getUniqueConstraint();
        uc.addColumn( getTransaction(), columnName );
        
        // Print success message
        print(MESSAGE_INDENT, getMessage(COLUMN_ADDED,columnName));
        
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
        if ( getArguments().isEmpty() ) {

            // find columns
            final KomodoObject parent = getContext().getParent(getTransaction());
            final String[] columnPaths = FindCommand.query( getWorkspaceStatus(), KomodoType.COLUMN, parent.getAbsolutePath(), null );

            if ( columnPaths.length == 0 ) {
                return -1;
            }

            if ( StringUtils.isBlank( lastArgument ) ) {
                candidates.addAll( Arrays.asList( columnPaths ) );
            } else {
                for ( final String item : Arrays.asList( columnPaths ) ) {
                    if ( item.startsWith( lastArgument ) ) {
                        candidates.add( item );
                    }
                }
            }

            return ( candidates.isEmpty() ? -1 : ( toString().length() + 1 ) );
        }

        // no completions if more than one arg
        return -1;
    }

}
