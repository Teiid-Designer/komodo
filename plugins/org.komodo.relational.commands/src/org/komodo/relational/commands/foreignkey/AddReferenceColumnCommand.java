/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.foreignkey;

import static org.komodo.relational.commands.foreignkey.ForeignKeyCommandMessages.AddReferenceColumnCommand.COLUMN_PATH_NOT_FOUND;
import static org.komodo.relational.commands.foreignkey.ForeignKeyCommandMessages.AddReferenceColumnCommand.COLUMN_REF_ADDED;
import static org.komodo.relational.commands.foreignkey.ForeignKeyCommandMessages.AddReferenceColumnCommand.INVALID_COLUMN;
import static org.komodo.relational.commands.foreignkey.ForeignKeyCommandMessages.AddReferenceColumnCommand.INVALID_COLUMN_PATH;
import static org.komodo.relational.commands.foreignkey.ForeignKeyCommandMessages.AddReferenceColumnCommand.MISSING_COLUMN_PATH;
import java.util.List;
import org.komodo.relational.commands.FindCommand;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Table;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.utils.StringUtils;

/**
 * A shell command to add a reference column to a {@link ForeignKey foreign key}.
 */
public final class AddReferenceColumnCommand extends ForeignKeyShellCommand {

    static final String NAME = "add-ref-column"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddReferenceColumnCommand( final WorkspaceStatus status ) {
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
            final String columnPath = requiredArgument( 0, getMessage( MISSING_COLUMN_PATH ) );

            // get reference of the column at the specified path
            final KomodoObject column = ContextUtils.getContextForPath( getWorkspaceStatus(), columnPath );

            if ( column == null ) {
                result = new CommandResultImpl( false, getMessage( COLUMN_PATH_NOT_FOUND, columnPath ), null );
            } else if ( column instanceof Column ) {
                final ForeignKey foreignKey = getForeignKey();

                // must NOT be a column in the parent of the table constraint
                final KomodoObject parentTable = foreignKey.getParent( getTransaction() );

                if ( parentTable.equals( column.getParent( getTransaction() ) ) ) {
                    result = new CommandResultImpl( false,
                                                    getMessage( INVALID_COLUMN,
                                                                getWorkspaceStatus().getLabelProvider()
                                                                                    .getDisplayPath( column.getAbsolutePath() ),
                                                                foreignKey.getName( getTransaction() ) ),
                                                    null );
                } else {
                    foreignKey.addReferencesColumn( getTransaction(), ( Column )column );
                    result = new CommandResultImpl( getMessage( COLUMN_REF_ADDED, columnPath, getContext().getAbsolutePath() ) );
                }
            } else {
                result = new CommandResultImpl( false, getMessage( INVALID_COLUMN_PATH, columnPath ), null );
            }
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
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        if ( getArguments().isEmpty() ) {
            final Repository.UnitOfWork uow = getTransaction();

            // find all columns in workspace
            final String[] allDisplayPaths = FindCommand.query( getWorkspaceStatus(), KomodoType.COLUMN, null, null );

            if ( allDisplayPaths.length == 0 ) {
                return -1;
            }

            final Table parent = getForeignKey().getTable( uow );
            final String parentPath = parent.getAbsolutePath();
            final String parentDisplayPath = getWorkspaceStatus().getLabelProvider().getDisplayPath( parentPath );

            // only add columns NOT found in the parent table
            for ( final String displayPath : allDisplayPaths ) {
                if ( !displayPath.startsWith( parentDisplayPath ) ) {
                    if ( StringUtils.isBlank( lastArgument ) || displayPath.startsWith( lastArgument ) ) {
                        candidates.add( displayPath );
                    }
                }
            }

            return ( candidates.isEmpty() ? -1 : ( toString().length() + 1 ) );
        }

        // no completions if more than one arg
        return -1;
    }

}
