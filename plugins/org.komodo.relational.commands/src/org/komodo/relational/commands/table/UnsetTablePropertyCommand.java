/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.table;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.UNSET_MISSING_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.UNSET_PROPERTY_SUCCESS;
import java.util.List;
import org.komodo.relational.model.Table;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.UnsetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

/**
 * A shell command to set Table properties
 */
public final class UnsetTablePropertyCommand extends TableShellCommand {

    static final String NAME = UnsetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public UnsetTablePropertyCommand( final WorkspaceStatus status ) {
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

            final Table table = getTable();
            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            switch ( name ) {
                case DESCRIPTION:
                    table.setDescription( transaction, null );
                    break;
                case CARDINALITY:
                    table.setCardinality( transaction, Table.DEFAULT_CARDINALITY );
                    break;
                case MATERIALIZED:
                    table.setMaterialized( transaction, Table.DEFAULT_MATERIALIZED );
                    break;
                case MATERIALIZED_TABLE:
                    table.setMaterializedTable( transaction, null );
                    break;
                case NAME_IN_SOURCE:
                    table.setNameInSource( transaction, null );
                    break;
                case UPDATABLE:
                    table.setUpdatable( transaction, Table.DEFAULT_UPDATABLE );
                    break;
                case UUID:
                    table.setUuid( transaction, null );
                    break;
                case ON_COMMIT_VALUE:
                    table.setOnCommitValue( transaction, null );
                    break;
                case QUERY_EXPRESSION:
                    table.setQueryExpression( transaction, null );
                    break;
                case SCHEMA_ELEMENT_TYPE:
                    table.setSchemaElementType( transaction, null );
                    break;
                case TEMPORARY_TABLE_TYPE:
                    table.setTemporaryTableType( transaction, null );
                    break;
                default:
                    errorMsg = getWorkspaceMessage( INVALID_PROPERTY_NAME, name, Table.class.getSimpleName() );
                    break;
            }

            if ( StringUtils.isBlank( errorMsg ) ) {
                result = new CommandResultImpl( getWorkspaceMessage( UNSET_PROPERTY_SUCCESS, name ) );
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
