/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.resultsetcolumn;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.UNSET_MISSING_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.UNSET_PROPERTY_SUCCESS;
import java.util.List;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.UnsetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

/**
 * A shell command to unset {@link Column column} properties.
 */
public final class UnsetResultSetColumnPropertyCommand extends ResultSetColumnShellCommand {

    static final String NAME = UnsetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public UnsetResultSetColumnPropertyCommand( final WorkspaceStatus status ) {
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
            final String name = requiredArgument( 0, getWorkspaceMessage( UNSET_MISSING_PROPERTY_NAME ) );
            final ResultSetColumn column = getResultSetColumn();
            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            switch ( name ) {
                case DATATYPE_NAME:
                    column.setDatatypeName( transaction, null );
                    break;
                case DEFAULT_VALUE:
                    column.setDefaultValue( transaction, null );
                    break;
                case DESCRIPTION:
                    column.setDescription( transaction, null );
                    break;
                case LENGTH:
                    column.setLength( transaction, RelationalConstants.DEFAULT_LENGTH );
                    break;
                case NAME_IN_SOURCE:
                    column.setNameInSource( transaction, null );
                    break;
                case NULLABLE:
                    column.setNullable( transaction, null );
                    break;
                case PRECISION:
                    column.setPrecision( transaction, RelationalConstants.DEFAULT_PRECISION );
                    break;
                case SCALE:
                    column.setScale( transaction, RelationalConstants.DEFAULT_SCALE );
                    break;
                case UUID:
                    column.setUuid( transaction, null );
                    break;
                default:
                    errorMsg = getWorkspaceMessage( INVALID_PROPERTY_NAME, name, Column.class.getSimpleName() );
                    break;
            }

            if ( StringUtils.isBlank( errorMsg ) ) {
                result = new CommandResultImpl( getWorkspaceMessage( UNSET_PROPERTY_SUCCESS, column.getName( transaction ), name ) );
            } else {
                result = new CommandResultImpl( false, errorMsg, null );
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
        final Arguments args = getArguments();

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( ALL_PROPS );
            } else {
                for ( final String item : ALL_PROPS ) {
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
