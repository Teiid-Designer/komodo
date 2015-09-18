/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import static org.komodo.relational.commands.WorkspaceCommandMessages.RemoveConstraintColumnCommand.COLUMN_PATH_NOT_FOUND;
import static org.komodo.relational.commands.WorkspaceCommandMessages.RemoveConstraintColumnCommand.COLUMN_REF_REMOVED;
import static org.komodo.relational.commands.WorkspaceCommandMessages.RemoveConstraintColumnCommand.INVALID_COLUMN_PATH;
import static org.komodo.relational.commands.WorkspaceCommandMessages.RemoveConstraintColumnCommand.MISSING_COLUMN_PATH;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.TableConstraint;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.utils.StringUtils;

/**
 * A shell command to remove a Column from a TableConstraint.
 */
public final class DeleteConstraintColumnCommand extends RelationalShellCommand {

    static final String NAME = "remove-constraint-column"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteConstraintColumnCommand( final WorkspaceStatus status ) {
        super( status, true, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String columnPathArg = requiredArgument( 0, getMessage( MISSING_COLUMN_PATH ) );

        // get selected column and remove
        final KomodoObject columnContext = ContextUtils.getContextForPath( getWorkspaceStatus(), columnPathArg );

        if ( columnContext == null ) {
            print( MESSAGE_INDENT, getMessage( COLUMN_PATH_NOT_FOUND, columnPathArg ) );
            return false;
        }

        final KomodoObject column = columnContext;

        if ( column instanceof Column ) {
            final TableConstraint constraint = getTableConstraint();
            constraint.removeColumn( getWorkspaceStatus().getTransaction(), ( Column )column );

            print( MESSAGE_INDENT, getMessage( COLUMN_REF_REMOVED, columnPathArg, getContext().getAbsolutePath() ) );
            return true;
        } else {
            print( MESSAGE_INDENT, getMessage( INVALID_COLUMN_PATH, columnPathArg ) );
            return false;
        }
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        // This command is valid for AccessPattern,ForeignKey,Index,PrimaryKey,UniqueConstraint
        try {
            KomodoType contextType = getContext().getTypeIdentifier(getTransaction());
            return (contextType==KomodoType.ACCESS_PATTERN || contextType==KomodoType.FOREIGN_KEY || 
                    contextType==KomodoType.INDEX || contextType==KomodoType.PRIMARY_KEY || contextType==KomodoType.UNIQUE_CONSTRAINT);
        } catch (Exception ex) {
            // on exception will return false
        }
        return false;
    }
    
    private TableConstraint getTableConstraint() {
        // initValidWsContextTypes() method assures execute is called only if current context is a TableConstraint
        final KomodoObject kobject = getContext();
        assert ( kobject instanceof TableConstraint );
        return ( TableConstraint )kobject;
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
            final TableConstraint constraint = getTableConstraint();
            final Column[] refCols = constraint.getColumns( getWorkspaceStatus().getTransaction() );

            // no tab-completion if no columns to remove
            if ( refCols.length == 0 ) {
                return -1;
            }

            // add matching paths of referenced columns
            final boolean noLastArg = StringUtils.isBlank( lastArgument );

            for ( final Column column : refCols ) {
                final String path = column.getAbsolutePath();

                if ( noLastArg || ( path.startsWith( lastArgument ) ) ) {
                    candidates.add( path );
                }
            }

            Collections.sort( candidates, new Comparator< CharSequence >() {

                /**
                 * {@inheritDoc}
                 *
                 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
                 */
                @Override
                public int compare( final CharSequence thisPath,
                                    final CharSequence thatPath ) {
                    return thisPath.toString().compareTo( thatPath.toString() );
                }
            });

            return ( candidates.isEmpty() ? -1 : ( toString().length() + 1 ) );
        }

        // no completions if more than one arg
        return -1;
    }
    
}
