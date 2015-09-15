/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model.view;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_BOOLEAN_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_INTEGER_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.relational.commands.model.table.TableCommandMessages.General.INVALID_ON_COMMIT_PROPERTY_VALUE;
import static org.komodo.relational.commands.model.table.TableCommandMessages.General.INVALID_SCHEMA_ELEMENT_TYPE_PROPERTY_VALUE;
import static org.komodo.relational.commands.model.table.TableCommandMessages.General.INVALID_TEMPORARY_TABLE_TYPE_PROPERTY_VALUE;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.model.SchemaElement;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.Table.OnCommit;
import org.komodo.relational.model.View;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.core.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to set Table properties
 */
public final class SetViewPropertyCommand extends ViewShellCommand {

    static final String NAME = "set-table-property"; //$NON-NLS-1$

    private static final String DESCRIPTION = "description"; //$NON-NLS-1$
    private static final String CARDINALITY = "cardinality"; //$NON-NLS-1$
    private static final String MATERIALIZED = "materialized"; //$NON-NLS-1$
    private static final String MATERIALIZED_TABLE = "materialized_table"; //$NON-NLS-1$
    private static final String NAME_IN_SOURCE = "nameinsource"; //$NON-NLS-1$
    private static final String UPDATABLE = "updatable"; //$NON-NLS-1$
    private static final String UUID = "uuid"; //$NON-NLS-1$
    private static final String ON_COMMIT_VALUE = "onCommitValue"; //$NON-NLS-1$
    private static final String QUERY_EXPRESSION = "queryExpression"; //$NON-NLS-1$
    private static final String SCHEMA_ELEMENT_TYPE = "schemaElementType"; //$NON-NLS-1$
    private static final String TEMPORARY_TABLE_TYPE = "temporary"; //$NON-NLS-1$

    private static final List< String > ALL_PROPS = Arrays.asList( new String[] { DESCRIPTION, CARDINALITY, MATERIALIZED, MATERIALIZED_TABLE,
                                                                                  NAME_IN_SOURCE, UPDATABLE, UUID, ON_COMMIT_VALUE,
                                                                                  QUERY_EXPRESSION, SCHEMA_ELEMENT_TYPE, TEMPORARY_TABLE_TYPE} );

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetViewPropertyCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
        // Overrides the BuiltInCommand "set-property"
        setOverriddenCommands(new String[]{SetPropertyCommand.NAME});
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        final String name = requiredArgument( 0, getWorkspaceMessage(MISSING_PROPERTY_NAME_VALUE) );
        final String value = requiredArgument( 1, getWorkspaceMessage(MISSING_PROPERTY_NAME_VALUE) );

        final View view = getView();
        
        final UnitOfWork transaction = getTransaction();
        boolean success = true;

//        switch ( name ) {
//            case DESCRIPTION:
//                table.setDescription(transaction, value);
//                break;
//            case CARDINALITY:
//                try {
//                    final int cardinality = Integer.parseInt( value );
//                    table.setCardinality(transaction, cardinality);
//                } catch ( final NumberFormatException e ) {
//                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, CARDINALITY ) );
//                    success = false;
//                }
//
//                break;
//            case MATERIALIZED:
//                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
//                    table.setMaterialized( transaction, Boolean.parseBoolean( value ) );
//                } else {
//                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, MATERIALIZED ) );
//                    success = false;
//                }
//
//                break;
//            case MATERIALIZED_TABLE:
//                table.setMaterializedTable(transaction, value);
//                break;
//            case NAME_IN_SOURCE:
//                table.setNameInSource(transaction, value);
//                break;
//            case UPDATABLE:
//                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
//                    table.setUpdatable( transaction, Boolean.parseBoolean( value ) );
//                } else {
//                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, UPDATABLE ) );
//                    success = false;
//                }
//
//                break;
//            case UUID:
//                table.setUuid(transaction, value);
//                break;
//            case ON_COMMIT_VALUE:
//                if ( OnCommit.DELETE_ROWS.name().equals( value ) ) {
//                    table.setOnCommitValue( transaction, OnCommit.DELETE_ROWS );
//                } else if ( OnCommit.PRESERVE_ROWS.name().equals( value ) ) {
//                    table.setOnCommitValue( transaction, OnCommit.PRESERVE_ROWS );
//                } else {
//                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_ON_COMMIT_PROPERTY_VALUE, ON_COMMIT_VALUE ) );
//                    success = false;
//                }
//                break;
//            case QUERY_EXPRESSION:
//                table.setQueryExpression(transaction, value);
//                break;
//            case SCHEMA_ELEMENT_TYPE:
//                if ( SchemaElement.SchemaElementType.FOREIGN.name().equals( value ) ) {
//                    table.setSchemaElementType( transaction, SchemaElement.SchemaElementType.FOREIGN );
//                } else if ( SchemaElement.SchemaElementType.VIRTUAL.name().equals( value ) ) {
//                    table.setSchemaElementType( transaction, SchemaElement.SchemaElementType.VIRTUAL );
//                } else {
//                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_SCHEMA_ELEMENT_TYPE_PROPERTY_VALUE, SCHEMA_ELEMENT_TYPE ) );
//                    success = false;
//                }
//                break;
//            case TEMPORARY_TABLE_TYPE:
//                if ( Table.TemporaryType.GLOBAL.name().equals( value ) ) {
//                    table.setTemporaryTableType( transaction, Table.TemporaryType.GLOBAL );
//                } else if ( Table.TemporaryType.LOCAL.name().equals( value ) ) {
//                    table.setTemporaryTableType( transaction, Table.TemporaryType.LOCAL );
//                } else {
//                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_TEMPORARY_TABLE_TYPE_PROPERTY_VALUE, TEMPORARY_TABLE_TYPE ) );
//                    success = false;
//                }
//                break;
//            default:
//                success = false;
//                print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_PROPERTY_NAME, NAME ) );
//                break;
//        }

        return success;
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
                candidates.add( Boolean.TRUE.toString() );
                candidates.add( Boolean.FALSE.toString() );
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

            return 0;
        }
        
        // no tab completion
        return -1;
    }

}
