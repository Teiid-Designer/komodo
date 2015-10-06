/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.userdefinedfunction;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.UNSET_MISSING_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.UNSET_PROPERTY_SUCCESS;
import java.util.List;
import org.komodo.relational.model.AbstractProcedure;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

/**
 * A shell command to unset {@link UserDefinedFunction user-defined function} properties.
 */
public final class UnsetUserDefinedFunctionPropertyCommand extends UserDefinedFunctionShellCommand {

    static final String NAME = SetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public UnsetUserDefinedFunctionPropertyCommand( final WorkspaceStatus status ) {
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

            final UserDefinedFunction func = getUserDefinedFunction();
            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            switch ( name ) {
                case AGGREGATE:
                    func.setAggregate( transaction, Function.DEFAULT_AGGREGATE );
                    break;
                case ALLOWS_DISTINCT:
                    func.setAllowsDistinct( transaction, Function.DEFAULT_ALLOWS_DISTINCT );
                    break;
                case ALLOWS_ORDERBY:
                    func.setAllowsOrderBy( transaction, Function.DEFAULT_ALLOWS_ORDER_BY );
                    break;
                case ANALYTIC:
                    func.setAnalytic( transaction, Function.DEFAULT_ANALYTIC );
                    break;
                case CATEGORY:
                    func.setCategory( getTransaction(), null );
                    break;
                case DECOMPOSABLE:
                    func.setDecomposable( transaction, Function.DEFAULT_DECOMPOSABLE );
                    break;
                case DESCRIPTION:
                    func.setDescription( getTransaction(), null );
                    break;
                case DETERMINISM:
                    func.setDeterminism( transaction, Function.Determinism.COMMAND_DETERMINISTIC );
                    break;
                case JAVA_CLASS:
                    func.setJavaClass( getTransaction(), null );
                    break;
                case JAVA_METHOD:
                    func.setJavaMethod( getTransaction(), null );
                    break;
                case NAME_IN_SOURCE:
                    func.setNameInSource( getTransaction(), null );
                    break;
                case NULL_ON_NULL:
                    func.setNullOnNull( transaction, Function.DEFAULT_NULL_ON_NULL );
                    break;
                case SCHEMA_ELEMENT_TYPE:
                    func.setSchemaElementType( transaction, null );
                    break;
                case UPDATE_COUNT:
                    func.setUpdateCount( transaction, AbstractProcedure.DEFAULT_UPDATE_COUNT );
                    break;
                case USES_DISTINCT_ROWS:
                    func.setUsesDistinctRows( transaction, Function.DEFAULT_USES_DISTINCT_ROWS );
                    break;
                case UUID:
                    func.setUuid( getTransaction(), null );
                    break;
                case VAR_ARGS:
                    func.setVarArgs( transaction, Function.DEFAULT_VARARGS );
                    break;
                default:
                    errorMsg = getWorkspaceMessage( INVALID_PROPERTY_NAME, name, UserDefinedFunction.class.getSimpleName() );
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
