/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.view;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_BOOLEAN_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_INTEGER_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.SET_PROPERTY_ERROR;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.SET_PROPERTY_SUCCESS;
import static org.komodo.relational.commands.view.ViewCommandMessages.General.INVALID_ON_COMMIT_PROPERTY_VALUE;
import static org.komodo.relational.commands.view.ViewCommandMessages.General.INVALID_SCHEMA_ELEMENT_TYPE_PROPERTY_VALUE;
import static org.komodo.relational.commands.view.ViewCommandMessages.General.INVALID_TEMPORARY_TABLE_TYPE_PROPERTY_VALUE;
import java.util.List;
import org.komodo.relational.model.SchemaElement;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.Table.OnCommit;
import org.komodo.relational.model.View;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

/**
 * A shell command to set {@link View view} properties.
 */
public final class SetViewPropertyCommand extends ViewShellCommand {

    static final String NAME = SetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetViewPropertyCommand( final WorkspaceStatus status ) {
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
            final String name = requiredArgument( 0, getWorkspaceMessage( MISSING_PROPERTY_NAME_VALUE ) );
            final String value = requiredArgument( 1, getWorkspaceMessage( MISSING_PROPERTY_NAME_VALUE ) );

            final View view = getView();
            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            switch ( name ) {
                case DESCRIPTION:
                    view.setDescription( transaction, value );
                    break;
                case CARDINALITY:
                    try {
                        final int cardinality = Integer.parseInt( value );
                        view.setCardinality( transaction, cardinality );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = getWorkspaceMessage( INVALID_INTEGER_PROPERTY_VALUE, CARDINALITY );
                    }

                    break;
                case MATERIALIZED:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        view.setMaterialized( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = getWorkspaceMessage( INVALID_BOOLEAN_PROPERTY_VALUE, MATERIALIZED );
                    }

                    break;
                case MATERIALIZED_TABLE:
                    view.setMaterializedTable( transaction, value );
                    break;
                case NAME_IN_SOURCE:
                    view.setNameInSource( transaction, value );
                    break;
                case UPDATABLE:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        view.setUpdatable( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = getWorkspaceMessage( INVALID_BOOLEAN_PROPERTY_VALUE, UPDATABLE );
                    }

                    break;
                case UUID:
                    view.setUuid( transaction, value );
                    break;
                case ON_COMMIT_VALUE:
                    if ( OnCommit.DELETE_ROWS.name().equals( value ) ) {
                        view.setOnCommitValue( transaction, OnCommit.DELETE_ROWS );
                    } else if ( OnCommit.PRESERVE_ROWS.name().equals( value ) ) {
                        view.setOnCommitValue( transaction, OnCommit.PRESERVE_ROWS );
                    } else {
                        errorMsg = getWorkspaceMessage( INVALID_ON_COMMIT_PROPERTY_VALUE, ON_COMMIT_VALUE );
                    }

                    break;
                case QUERY_EXPRESSION:
                    view.setQueryExpression( transaction, value );
                    break;
                case SCHEMA_ELEMENT_TYPE:
                    if ( SchemaElement.SchemaElementType.FOREIGN.name().equals( value ) ) {
                        view.setSchemaElementType( transaction, SchemaElement.SchemaElementType.FOREIGN );
                    } else if ( SchemaElement.SchemaElementType.VIRTUAL.name().equals( value ) ) {
                        view.setSchemaElementType( transaction, SchemaElement.SchemaElementType.VIRTUAL );
                    } else {
                        errorMsg = getWorkspaceMessage( INVALID_SCHEMA_ELEMENT_TYPE_PROPERTY_VALUE, SCHEMA_ELEMENT_TYPE );
                    }

                    break;
                case TEMPORARY_TABLE_TYPE:
                    if ( Table.TemporaryType.GLOBAL.name().equals( value ) ) {
                        view.setTemporaryTableType( transaction, Table.TemporaryType.GLOBAL );
                    } else if ( Table.TemporaryType.LOCAL.name().equals( value ) ) {
                        view.setTemporaryTableType( transaction, Table.TemporaryType.LOCAL );
                    } else {
                        errorMsg = getWorkspaceMessage( INVALID_TEMPORARY_TABLE_TYPE_PROPERTY_VALUE, TEMPORARY_TABLE_TYPE );
                    }

                    break;
                default:
                    errorMsg = getWorkspaceMessage( INVALID_PROPERTY_NAME, name, View.class.getSimpleName() );
                    break;
            }

            if ( StringUtils.isBlank( errorMsg ) ) {
                result = new CommandResultImpl( getMessage( SET_PROPERTY_SUCCESS, name ) );
            } else {
                result = new CommandResultImpl( false, errorMsg, null );
            }
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( SET_PROPERTY_ERROR ), e );
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
        return 2;
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

        if ( ( args.size() == 1 ) ) {
            String theArg = getArguments().get(0);
            if( MATERIALIZED.equals(theArg) || UPDATABLE.equals(theArg) ) {
                updateCandidatesForBooleanProperty( lastArgument, candidates );
            } else if( ON_COMMIT_VALUE.equals(theArg) ) {
                candidates.add( OnCommit.DELETE_ROWS.name() );
                candidates.add( OnCommit.PRESERVE_ROWS.name() );
            } else if( SCHEMA_ELEMENT_TYPE.equals(theArg)) {
                candidates.add( SchemaElement.SchemaElementType.FOREIGN.name() );
                candidates.add( SchemaElement.SchemaElementType.VIRTUAL.name() );
            } else if( TEMPORARY_TABLE_TYPE.equals(theArg)) {
                candidates.add( Table.TemporaryType.GLOBAL.name() );
                candidates.add( Table.TemporaryType.LOCAL.name() );
            }

            return ( candidates.isEmpty() ? -1 : ( StringUtils.isBlank( lastArgument ) ? 0 : ( toString().length() + 1 ) ) );
        }

        // no tab completion
        return -1;
    }

}
