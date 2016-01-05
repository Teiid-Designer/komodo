/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.tableconstraint;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.KomodoObjectLabelProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to add a column to a {@link TableConstraint}.
 */
public final class AddConstraintColumnCommand extends TableConstraintShellCommand {

    static final String NAME = "add-column"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddConstraintColumnCommand( final WorkspaceStatus status ) {
        super( status, NAME );
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
            final String columnPath = requiredArgument( 0, I18n.bind( TableConstraintCommandsI18n.missingColumnPathForAdd ) );

            // Validate the display Path
            String validationMsg = validatePath(columnPath);
            if(!validationMsg.equals(CompletionConstants.OK)) {
                return new CommandResultImpl(false, validationMsg, null);
            }

            // Get the Object at the supplied path
            KomodoObject possible = getWorkspaceStatus().getContextForDisplayPath(columnPath.trim());

            Column column = null;
            { // see if valid column
                try {
                    if ( Column.RESOLVER.resolvable( getTransaction(), possible ) ) {
                        column = Column.RESOLVER.resolve( getTransaction(), possible );
                    } else {
                        result = new CommandResultImpl( false,
                                                        I18n.bind( TableConstraintCommandsI18n.invalidColumnPath, columnPath ),
                                                        null );
                    }
                } catch ( final Exception e ) {
                    result = new CommandResultImpl( false,
                                                    I18n.bind( TableConstraintCommandsI18n.invalidColumnPath, columnPath ),
                                                    null );
                }
            }

            if(column!=null) {
                final TableConstraint constraint = getTableConstraint();

                // must be a column in the parent of the table constraint
                final KomodoObject parentTable = constraint.getParent( getTransaction() );

                if ( parentTable.equals( column.getParent( getTransaction() ) ) ) {
                    constraint.addColumn( getTransaction(), column );
                    result = new CommandResultImpl( I18n.bind( TableConstraintCommandsI18n.columnRefAdded,
                                                               columnPath,
                                                               getWorkspaceStatus().getCurrentContextDisplayPath() ) );
                } else {
                    result = new CommandResultImpl( false,
                                                    I18n.bind( TableConstraintCommandsI18n.invalidColumn,
                                                                getWorkspaceStatus().getDisplayPath( column ),
                                                                constraint.getName( getTransaction() ) ),
                                                    null );
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
        print( indent, I18n.bind( TableConstraintCommandsI18n.addConstraintColumnHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( TableConstraintCommandsI18n.addConstraintColumnExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( TableConstraintCommandsI18n.addConstraintColumnUsage ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        if ( getArguments().isEmpty() ) {
            final Repository.UnitOfWork uow = getTransaction();

            // can only add columns of the parent table
            final Table parent = getTableConstraint().getTable( uow );
            final Column[] columns = parent.getColumns( uow );

            if ( columns.length == 0 ) {
                return TabCompletionModifier.AUTO;
            }

            final KomodoObjectLabelProvider labelProvider = getWorkspaceStatus().getCurrentContextLabelProvider();

            for ( final Column column : Arrays.asList( columns ) ) {
                final String absolutePath = column.getAbsolutePath();
                final String displayPath = labelProvider.getDisplayPath( column );

                if ( StringUtils.isBlank( lastArgument )
                     || absolutePath.startsWith( lastArgument )
                     || displayPath.startsWith( lastArgument ) ) {
                    candidates.add( displayPath );
                }
            }
        }

        // no completions if more than one arg
        return TabCompletionModifier.AUTO;
    }

}
