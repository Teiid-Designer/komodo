/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.parameter;

import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.INVALID_BOOLEAN_PROPERTY_VALUE;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.INVALID_INTEGER_PROPERTY_VALUE;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.relational.commands.workspace.WorkspaceCommandMessages.General.SET_PROPERTY_SUCCESS;
import static org.komodo.relational.commands.parameter.ParameterCommandMessages.General.INVALID_DIRECTION_PROPERTY_VALUE;
import static org.komodo.relational.commands.parameter.ParameterCommandMessages.General.INVALID_NULLABLE_PROPERTY_VALUE;
import java.util.List;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Parameter.Direction;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

/**
 * A shell command to set {@link Parameter parameter} properties.
 */
public final class SetParameterPropertyCommand extends ParameterShellCommand {

    static final String NAME = SetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetParameterPropertyCommand( final WorkspaceStatus status ) {
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

            final Parameter parameter = getParameter();
            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            switch ( name ) {
                case DATATYPE_NAME:
                    parameter.setDatatypeName( transaction, value );
                    break;
                case DEFAULT_VALUE:
                    parameter.setDefaultValue( transaction, value );
                    break;
                case DIRECTION:
                    if ( Direction.IN.name().equals( value ) ) {
                        parameter.setDirection( transaction, Direction.IN );
                    } else if ( Direction.IN_OUT.name().equals( value ) ) {
                        parameter.setDirection( transaction, Direction.IN_OUT );
                    } else if ( Direction.OUT.name().equals( value ) ) {
                        parameter.setDirection( transaction, Direction.OUT );
                    } else if ( Direction.VARIADIC.name().equals( value ) ) {
                        parameter.setDirection( transaction, Direction.VARIADIC );
                    } else {
                        errorMsg = getWorkspaceMessage( INVALID_DIRECTION_PROPERTY_VALUE, NULLABLE );
                    }

                    break;
                case LENGTH:
                    try {
                        final long length = Long.parseLong( value );
                        parameter.setLength( transaction, length );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = getWorkspaceMessage( INVALID_INTEGER_PROPERTY_VALUE, LENGTH );
                    }

                    break;
                case NULLABLE:
                    if ( Nullable.NO_NULLS.name().equals( value ) ) {
                        parameter.setNullable( transaction, Nullable.NO_NULLS );
                    } else if ( Nullable.NULLABLE.name().equals( value ) ) {
                        parameter.setNullable( transaction, Nullable.NULLABLE );
                    } else if ( Nullable.NULLABLE_UNKNOWN.name().equals( value ) ) {
                        parameter.setNullable( transaction, Nullable.NULLABLE_UNKNOWN );
                    } else {
                        errorMsg = getWorkspaceMessage( INVALID_NULLABLE_PROPERTY_VALUE, NULLABLE );
                    }

                    break;
                case PRECISION:
                    try {
                        final long precision = Long.parseLong( value );
                        parameter.setPrecision( transaction, precision );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = getWorkspaceMessage( INVALID_INTEGER_PROPERTY_VALUE, PRECISION );
                    }

                    break;
                case RESULT:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        parameter.setResult( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = getWorkspaceMessage( INVALID_BOOLEAN_PROPERTY_VALUE, RESULT );
                    }

                    break;
                case SCALE:
                    try {
                        final long scale = Long.parseLong( value );
                        parameter.setScale( transaction, scale );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = getWorkspaceMessage( INVALID_INTEGER_PROPERTY_VALUE, SCALE );
                    }

                    break;
                default:
                    errorMsg = getWorkspaceMessage( INVALID_PROPERTY_NAME, name, Parameter.class.getSimpleName() );
                    break;
            }

            if ( StringUtils.isBlank( errorMsg ) ) {
                result = new CommandResultImpl( getWorkspaceMessage( SET_PROPERTY_SUCCESS, name ) );
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
            if( RESULT.equals(theArg) ) {
                updateCandidatesForBooleanProperty( lastArgument, candidates );
            } else if( NULLABLE.equals(theArg) ) {
                candidates.add( Nullable.NO_NULLS.name() );
                candidates.add( Nullable.NULLABLE.name() );
                candidates.add( Nullable.NULLABLE_UNKNOWN.name() );
            } else if( DIRECTION.equals(theArg)) {
                candidates.add( Direction.IN.name() );
                candidates.add( Direction.IN_OUT.name() );
                candidates.add( Direction.OUT.name() );
                candidates.add( Direction.VARIADIC.name() );
            }

            return ( candidates.isEmpty() ? -1 : ( StringUtils.isBlank( lastArgument ) ? 0 : ( toString().length() + 1 ) ) );
        }

        // no tab completion
        return -1;
    }

}
