/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model.pushdownfunction;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.model.Parameter.Direction;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.core.SetPropertyCommand;

/**
 * A shell command to set PushdownFunction properties
 */
public final class SetPushdownFunctionPropertyCommand extends PushdownFunctionShellCommand {

    static final String NAME = "set-pushdown-function-property"; //$NON-NLS-1$

    private static final String AGGREGATE = "aggregate"; //$NON-NLS-1$
    private static final String ALLOWS_DISTINCT = "allows-distinct"; //$NON-NLS-1$
    private static final String ALLOWS_ORDERBY = "allows-orderby"; //$NON-NLS-1$
    private static final String ANALYTIC = "analytic"; //$NON-NLS-1$
    private static final String DECOMPOSABLE = "decomposable"; //$NON-NLS-1$
    private static final String DESCRIPTION = "description"; //$NON-NLS-1$
    private static final String DETERMINISM = "determinism"; //$NON-NLS-1$
    private static final String NAME_IN_SOURCE = "name-in-source"; //$NON-NLS-1$
    private static final String NULL_ON_NULL = "null-on-null"; //$NON-NLS-1$
    private static final String PRIMARY_TYPE = "primary-type"; //$NON-NLS-1$
    private static final String RESULT_SET = "result-set"; //$NON-NLS-1$
    private static final String SCHEMA_ELEMENT_TYPE = "schema-element-type"; //$NON-NLS-1$
    private static final String UPDATE_COUNT = "update-count"; //$NON-NLS-1$
    private static final String USES_DISTINCT_ROWS = "uses-distinct-rows"; //$NON-NLS-1$
    private static final String UUID = "uuid"; //$NON-NLS-1$
    private static final String VAR_ARGS = "var-args"; //$NON-NLS-1$

    private static final List< String > ALL_PROPS = Arrays.asList( new String[] { AGGREGATE, ALLOWS_DISTINCT, ALLOWS_ORDERBY,
                                                                                  ANALYTIC, DECOMPOSABLE, DESCRIPTION, DETERMINISM,
                                                                                  NAME_IN_SOURCE, NULL_ON_NULL, PRIMARY_TYPE, RESULT_SET,
                                                                                  SCHEMA_ELEMENT_TYPE, UPDATE_COUNT, USES_DISTINCT_ROWS, UUID, VAR_ARGS} );
    
    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetPushdownFunctionPropertyCommand( final WorkspaceStatus status ) {
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
//        final String name = requiredArgument( 0, getWorkspaceMessage(MISSING_PROPERTY_NAME_VALUE) );
//        final String value = requiredArgument( 1, getWorkspaceMessage(MISSING_PROPERTY_NAME_VALUE) );
//
//        final PushdownFunction func = getPushdownFunction();
//        func.setAggregate(transaction, newAggregate);
//        func.setAllowsDistinct(transaction, newAllowsDistinct);
//        func.setAllowsOrderBy(transaction, newAllowsOrderBy);
//        func.setAnalytic(transaction, newIsAnalytic);
//        func.setDecomposable(transaction, newDecomposable);
//        func.setDescription(transaction, newDescription);
//        func.setDeterminism(transaction, newDeterminism);
//        func.setNameInSource(transaction, newNameInSource);
//        func.setNullOnNull(transaction, newNullOnNull);
//        func.setPrimaryType(transaction, typeName);
//        func.setResultSet();
//        func.setSchemaElementType(transaction, newSchemaElementType);
//        func.setUpdateCount(transaction, newUpdateCount);
//        func.setUsesDistinctRows(transaction, newUsesDistinctRows);
//        func.setUuid(transaction, newUuid);
//        func.setVarArgs(transaction, newVarArgs);
//        
//        final UnitOfWork transaction = getTransaction();
        boolean success = true;

//        switch ( name ) {
//            case DATATYPE_NAME:
//                parameter.setDatatypeName(transaction, value);
//                break;
//            case DEFAULT_VALUE:
//                parameter.setDefaultValue(transaction, value);
//                break;
//            case DIRECTION:
//                if ( Direction.IN.name().equals( value ) ) {
//                    parameter.setDirection( transaction, Direction.IN );
//                } else if ( Direction.IN_OUT.name().equals( value ) ) {
//                    parameter.setDirection( transaction, Direction.IN_OUT );
//                } else if ( Direction.OUT.name().equals( value ) ) {
//                    parameter.setDirection( transaction, Direction.OUT );
//                } else if ( Direction.VARIADIC.name().equals( value ) ) {
//                    parameter.setDirection( transaction, Direction.VARIADIC );
//                } else {
//                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_DIRECTION_PROPERTY_VALUE, NULLABLE ) );
//                    success = false;
//                }
//                break;
//            case LENGTH:
//                try {
//                    final long length = Long.parseLong( value );
//                    parameter.setLength(transaction, length);
//                } catch ( final NumberFormatException e ) {
//                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, LENGTH ) );
//                    success = false;
//                }
//
//                break;
//            case NULLABLE:
//                if ( Nullable.NO_NULLS.name().equals( value ) ) {
//                    parameter.setNullable( transaction, Nullable.NO_NULLS );
//                } else if ( Nullable.NULLABLE.name().equals( value ) ) {
//                    parameter.setNullable( transaction, Nullable.NULLABLE );
//                } else if ( Nullable.NULLABLE_UNKNOWN.name().equals( value ) ) {
//                    parameter.setNullable( transaction, Nullable.NULLABLE_UNKNOWN );
//                } else {
//                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_NULLABLE_PROPERTY_VALUE, NULLABLE ) );
//                    success = false;
//                }
//                break;
//            case PRECISION:
//                try {
//                    final long precision = Long.parseLong( value );
//                    parameter.setPrecision(transaction, precision);
//                } catch ( final NumberFormatException e ) {
//                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, PRECISION ) );
//                    success = false;
//                }
//
//                break;
//            case PRIMARY_TYPE:
//                parameter.setPrimaryType(transaction, value);
//                break;
//            case RESULT:
//                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
//                    parameter.setResult( transaction, Boolean.parseBoolean( value ) );
//                } else {
//                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, RESULT ) );
//                    success = false;
//                }
//
//                break;
//            case SCALE:
//                try {
//                    final long scale = Long.parseLong( value );
//                    parameter.setScale(transaction, scale);
//                } catch ( final NumberFormatException e ) {
//                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, SCALE ) );
//                    success = false;
//                }
//
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

//        if ( ( args.size() == 1 ) ) {
//            String theArg = getArguments().get(0);
//            if( RESULT.equals(theArg) ) {
//                candidates.add( Boolean.TRUE.toString() );
//                candidates.add( Boolean.FALSE.toString() );
//            } else if( NULLABLE.equals(theArg) ) {
//                candidates.add( Nullable.NO_NULLS.name() );
//                candidates.add( Nullable.NULLABLE.name() );
//                candidates.add( Nullable.NULLABLE_UNKNOWN.name() );
//            } else if( DIRECTION.equals(theArg)) {
//                candidates.add( Direction.IN.name() );
//                candidates.add( Direction.IN_OUT.name() );
//                candidates.add( Direction.OUT.name() );
//                candidates.add( Direction.VARIADIC.name() );
//            }
//
//            return 0;
//        }
        
        // no tab completion
        return -1;
    }

}
