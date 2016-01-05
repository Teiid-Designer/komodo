/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.tableconstraint;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.TableConstraint;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to remove a column from a {@link TableConstraint}.
 */
public final class DeleteConstraintColumnCommand extends TableConstraintShellCommand {

    static final String NAME = "delete-column"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public DeleteConstraintColumnCommand( final WorkspaceStatus status ) {
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
            final String columnPathArg = requiredArgument( 0, I18n.bind( TableConstraintCommandsI18n.missingColumnPathForDelete ) );

            // Validate the display Path
            String validationMsg = validatePath(columnPathArg);
            if(!validationMsg.equals(CompletionConstants.OK)) {
                return new CommandResultImpl(false, validationMsg, null);
            }

            // Get the Object at the supplied path
            KomodoObject possible = getWorkspaceStatus().getContextForDisplayPath(columnPathArg.trim());

            Column column = null;
            { // see if valid column
                try {
                    if ( Column.RESOLVER.resolvable( getTransaction(), possible ) ) {
                        column = Column.RESOLVER.resolve( getTransaction(), possible );
                    } else {
                        result = new CommandResultImpl( false,
                                                        I18n.bind( TableConstraintCommandsI18n.invalidColumnPath, columnPathArg ),
                                                        null );
                    }
                } catch ( final Exception e ) {
                    result = new CommandResultImpl( false,
                                                    I18n.bind( TableConstraintCommandsI18n.invalidColumnPath, columnPathArg ),
                                                    null );
                }
            }

            if ( column != null ) {
                final TableConstraint constraint = getTableConstraint();
                constraint.removeColumn( getTransaction(), column );

                result = new CommandResultImpl( I18n.bind( TableConstraintCommandsI18n.columnRemoved,
                                                            columnPathArg,
                                                            getContext().getAbsolutePath() ) );
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
        print( indent, I18n.bind( TableConstraintCommandsI18n.deleteConstraintColumnHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( TableConstraintCommandsI18n.deleteConstraintColumnExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( TableConstraintCommandsI18n.deleteConstraintColumnUsage ) );
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
            final TableConstraint constraint = getTableConstraint();
            final Column[] refCols = constraint.getColumns( getTransaction() );

            // no tab-completion if no columns to remove
            if ( refCols.length == 0 ) {
                return TabCompletionModifier.AUTO;
            }

            // add matching paths of referenced columns
            final boolean noLastArg = StringUtils.isBlank( lastArgument );

            for ( final Column column : refCols ) {
                final String displayPath = getWorkspaceStatus().getCurrentContextLabelProvider().getDisplayPath( column );
                final String absolutePath = column.getAbsolutePath();

                if ( noLastArg || displayPath.startsWith( lastArgument ) || absolutePath.startsWith( lastArgument ) ) {
                    candidates.add( displayPath );
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
            } );
        }

        // no completions if more than one arg
        return TabCompletionModifier.AUTO;
    }

}
