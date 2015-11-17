/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.foreignkey;

import java.util.List;
import org.komodo.relational.commands.FindCommand;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Table;
import org.komodo.repository.ObjectImpl;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

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
            final String columnPath = requiredArgument( 0, I18n.bind( ForeignKeyCommandsI18n.missingColumnPathForAdd ) );

            // get reference of the column at the specified path
            Column column = null;
            { // see if valid column
                String repoPath = getWorkspaceStatus().getLabelProvider().getPath( columnPath );

                if ( StringUtils.isBlank( repoPath ) ) {
                    repoPath = columnPath;
                }

                final KomodoObject possible = new ObjectImpl( getRepository(), repoPath, 0 );

                try {
                    if ( Column.RESOLVER.resolvable( getTransaction(), possible ) ) {
                        column = Column.RESOLVER.resolve( getTransaction(), possible );
                    } else {
                        result = new CommandResultImpl( false,
                                                        I18n.bind( ForeignKeyCommandsI18n.invalidColumnPath, columnPath ),
                                                        null );
                    }
                } catch ( final Exception e ) {
                    result = new CommandResultImpl( false, I18n.bind( ForeignKeyCommandsI18n.invalidColumnPath, columnPath ), e );
                }
            }

            if ( column != null ) {
                final ForeignKey foreignKey = getForeignKey();

                // must NOT be a column in the parent of the table constraint
                final KomodoObject parentTable = foreignKey.getParent( getTransaction() );

                if ( parentTable.equals( column.getParent( getTransaction() ) ) ) {
                    result = new CommandResultImpl( false,
                                                    I18n.bind( ForeignKeyCommandsI18n.invalidColumn,
                                                               getWorkspaceStatus().getLabelProvider()
                                                                                   .getDisplayPath( column.getAbsolutePath() ),
                                                               foreignKey.getName( getTransaction() ) ),
                                                    null );
                } else {
                    foreignKey.addReferencesColumn( getTransaction(), column );
                    result = new CommandResultImpl( I18n.bind( ForeignKeyCommandsI18n.columnRefAdded,
                                                               columnPath,
                                                               getContext().getAbsolutePath() ) );
                }
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
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ForeignKeyCommandsI18n.addReferenceColumnHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ForeignKeyCommandsI18n.addReferenceColumnExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ForeignKeyCommandsI18n.addReferenceColumnUsage ) );
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
