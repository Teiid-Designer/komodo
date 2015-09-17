/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.userdefinedfunction;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_BOOLEAN_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_INTEGER_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.relational.commands.userdefinedfunction.UserDefinedFunctionCommandMessages.General.INVALID_DETERMINISTIC_PROPERTY_VALUE;
import static org.komodo.relational.commands.userdefinedfunction.UserDefinedFunctionCommandMessages.General.INVALID_SCHEMA_ELEMENT_TYPE_PROPERTY_VALUE;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.SchemaElement;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.core.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to set UserDefinedFunction properties
 */
public final class SetUserDefinedFunctionPropertyCommand extends UserDefinedFunctionShellCommand {

    static final String NAME = "set-user-defined-function-property"; //$NON-NLS-1$

    private static final String AGGREGATE = "aggregate"; //$NON-NLS-1$
    private static final String ALLOWS_DISTINCT = "allows-distinct"; //$NON-NLS-1$
    private static final String ALLOWS_ORDERBY = "allows-orderby"; //$NON-NLS-1$
    private static final String ANALYTIC = "analytic"; //$NON-NLS-1$
    private static final String CATEGORY = "category"; //$NON-NLS-1$
    private static final String DECOMPOSABLE = "decomposable"; //$NON-NLS-1$
    private static final String DESCRIPTION = "description"; //$NON-NLS-1$
    private static final String DETERMINISM = "determinism"; //$NON-NLS-1$
    private static final String JAVA_CLASS = "java-class"; //$NON-NLS-1$
    private static final String JAVA_METHOD = "java-method"; //$NON-NLS-1$
    private static final String NAME_IN_SOURCE = "name-in-source"; //$NON-NLS-1$
    private static final String NULL_ON_NULL = "null-on-null"; //$NON-NLS-1$
    private static final String SCHEMA_ELEMENT_TYPE = "schema-element-type"; //$NON-NLS-1$
    private static final String UPDATE_COUNT = "update-count"; //$NON-NLS-1$
    private static final String USES_DISTINCT_ROWS = "uses-distinct-rows"; //$NON-NLS-1$
    private static final String UUID = "uuid"; //$NON-NLS-1$
    private static final String VAR_ARGS = "var-args"; //$NON-NLS-1$

    private static final List< String > ALL_PROPS = Arrays.asList( new String[] { AGGREGATE, ALLOWS_DISTINCT, ALLOWS_ORDERBY,
                                                                                  ANALYTIC, CATEGORY, DECOMPOSABLE, DESCRIPTION, DETERMINISM,
                                                                                  JAVA_CLASS, JAVA_METHOD, NAME_IN_SOURCE, NULL_ON_NULL, 
                                                                                  SCHEMA_ELEMENT_TYPE,
                                                                                  UPDATE_COUNT, USES_DISTINCT_ROWS, UUID, VAR_ARGS} );
    
    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetUserDefinedFunctionPropertyCommand( final WorkspaceStatus status ) {
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

        final UserDefinedFunction func = getUserDefinedFunction();
        final UnitOfWork transaction = getTransaction();
        boolean success = true;

        switch ( name ) {
            case AGGREGATE:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    func.setAggregate( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, AGGREGATE ) );
                    success = false;
                }

                break;
            case ALLOWS_DISTINCT:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    func.setAllowsDistinct( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, ALLOWS_DISTINCT ) );
                    success = false;
                }

                break;
            case ALLOWS_ORDERBY:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    func.setAllowsOrderBy( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, ALLOWS_ORDERBY ) );
                    success = false;
                }

                break;
            case ANALYTIC:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    func.setAnalytic( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, ANALYTIC ) );
                    success = false;
                }

                break;
            case CATEGORY:
                func.setCategory(getTransaction(), value);
                break;
            case DECOMPOSABLE:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    func.setDecomposable( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, DECOMPOSABLE ) );
                    success = false;
                }

                break;
            case DESCRIPTION:
                func.setDescription(getTransaction(), value);
                break;
            case DETERMINISM:
                if ( Function.Determinism.COMMAND_DETERMINISTIC.name().equals( value ) ) {
                    func.setDeterminism( transaction, Function.Determinism.COMMAND_DETERMINISTIC );
                } else if ( Function.Determinism.DETERMINISTIC.name().equals( value ) ) {
                    func.setDeterminism( transaction, Function.Determinism.DETERMINISTIC );
                } else if ( Function.Determinism.NONDETERMINISTIC.name().equals( value ) ) {
                    func.setDeterminism( transaction, Function.Determinism.NONDETERMINISTIC );
                } else if ( Function.Determinism.SESSION_DETERMINISTIC.name().equals( value ) ) {
                    func.setDeterminism( transaction, Function.Determinism.SESSION_DETERMINISTIC );
                } else if ( Function.Determinism.USER_DETERMINISTIC.name().equals( value ) ) {
                    func.setDeterminism( transaction, Function.Determinism.USER_DETERMINISTIC );
                } else if ( Function.Determinism.VDB_DETERMINISTIC.name().equals( value ) ) {
                    func.setDeterminism( transaction, Function.Determinism.VDB_DETERMINISTIC );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_DETERMINISTIC_PROPERTY_VALUE, DETERMINISM ) );
                    success = false;
                }
                break;
            case JAVA_CLASS:
                func.setJavaClass(getTransaction(), value);
                break;
            case JAVA_METHOD:
                func.setJavaMethod(getTransaction(), value);
                break;
            case NAME_IN_SOURCE:
                func.setNameInSource(getTransaction(), value);
                break;
            case NULL_ON_NULL:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    func.setNullOnNull( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, NULL_ON_NULL ) );
                    success = false;
                }

                break;
            case SCHEMA_ELEMENT_TYPE:
                if ( SchemaElement.SchemaElementType.FOREIGN.name().equals( value ) ) {
                    func.setSchemaElementType( transaction, SchemaElement.SchemaElementType.FOREIGN );
                } else if ( SchemaElement.SchemaElementType.VIRTUAL.name().equals( value ) ) {
                    func.setSchemaElementType( transaction, SchemaElement.SchemaElementType.VIRTUAL );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_SCHEMA_ELEMENT_TYPE_PROPERTY_VALUE, SCHEMA_ELEMENT_TYPE ) );
                    success = false;
                }
                break;
            case UPDATE_COUNT:
                try {
                    final long count = Long.parseLong( value );
                    func.setUpdateCount(transaction, count);
                } catch ( final NumberFormatException e ) {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, UPDATE_COUNT ) );
                    success = false;
                }

                break;
            case USES_DISTINCT_ROWS:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    func.setUsesDistinctRows( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, USES_DISTINCT_ROWS ) );
                    success = false;
                }

                break;
            case UUID:
                func.setUuid(getTransaction(), value);
                break;
            case VAR_ARGS:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    func.setVarArgs( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, VAR_ARGS ) );
                    success = false;
                }

                break;
            default:
                success = false;
                print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_PROPERTY_NAME, NAME ) );
                break;
        }

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
            if(    AGGREGATE.equals(theArg) || ALLOWS_DISTINCT.equals(theArg) || ALLOWS_ORDERBY.equals(theArg) || ANALYTIC.equals(theArg) 
                || DECOMPOSABLE.equals(theArg) || NULL_ON_NULL.equals(theArg) || USES_DISTINCT_ROWS.equals(theArg) || VAR_ARGS.equals(theArg)) {
                candidates.add( Boolean.TRUE.toString() );
                candidates.add( Boolean.FALSE.toString() );
            } else if( DETERMINISM.equals(theArg) ) {
                candidates.add( Function.Determinism.COMMAND_DETERMINISTIC.name() );
                candidates.add( Function.Determinism.DETERMINISTIC.name() );
                candidates.add( Function.Determinism.NONDETERMINISTIC.name() );
                candidates.add( Function.Determinism.SESSION_DETERMINISTIC.name() );
                candidates.add( Function.Determinism.USER_DETERMINISTIC.name() );
                candidates.add( Function.Determinism.VDB_DETERMINISTIC.name() );
            } else if( SCHEMA_ELEMENT_TYPE.equals(theArg)) {
                candidates.add( SchemaElement.SchemaElementType.FOREIGN.name() );
                candidates.add( SchemaElement.SchemaElementType.VIRTUAL.name() );
            }

            return 0;
        }
        
        // no tab completion
        return -1;
    }
}
