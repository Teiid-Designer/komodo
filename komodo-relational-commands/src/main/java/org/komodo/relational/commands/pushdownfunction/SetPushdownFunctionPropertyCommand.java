/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational.commands.pushdownfunction;

import java.util.List;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.SchemaElement;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to set PushdownFunction properties
 */
public final class SetPushdownFunctionPropertyCommand extends PushdownFunctionShellCommand {

    static final String NAME = SetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetPushdownFunctionPropertyCommand( final WorkspaceStatus status ) {
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
            final String name = requiredArgument( 0, I18n.bind( WorkspaceCommandsI18n.missingPropertyNameValue ) );
            final String value = requiredArgument( 1, I18n.bind( WorkspaceCommandsI18n.missingPropertyNameValue ) );

            final PushdownFunction func = getPushdownFunction();
            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            switch ( name ) {
                case AGGREGATE:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        func.setAggregate( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, AGGREGATE );
                    }

                    break;
                case ALLOWS_DISTINCT:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        func.setAllowsDistinct( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOWS_DISTINCT );
                    }

                    break;
                case ALLOWS_ORDERBY:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        func.setAllowsOrderBy( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOWS_ORDERBY );
                    }

                    break;
                case ANALYTIC:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        func.setAnalytic( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ANALYTIC );
                    }

                    break;
                case DECOMPOSABLE:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        func.setDecomposable( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, DECOMPOSABLE );
                    }

                    break;
                case DESCRIPTION:
                    func.setDescription( getTransaction(), value );
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
                        errorMsg = I18n.bind( PushdownFunctionCommandsI18n.invalidDeterministicPropertyValue, DETERMINISM );
                    }

                    break;
                case NAME_IN_SOURCE:
                    func.setNameInSource( getTransaction(), value );
                    break;
                case NULL_ON_NULL:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        func.setNullOnNull( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, NULL_ON_NULL );
                    }

                    break;
                case SCHEMA_ELEMENT_TYPE:
                    if ( SchemaElement.SchemaElementType.FOREIGN.name().equals( value ) ) {
                        func.setSchemaElementType( transaction, SchemaElement.SchemaElementType.FOREIGN );
                    } else if ( SchemaElement.SchemaElementType.VIRTUAL.name().equals( value ) ) {
                        func.setSchemaElementType( transaction, SchemaElement.SchemaElementType.VIRTUAL );
                    } else {
                        errorMsg = I18n.bind( PushdownFunctionCommandsI18n.invalidSchemaElementTypePropertyValue, SCHEMA_ELEMENT_TYPE );
                    }

                    break;
                case UPDATE_COUNT:
                    try {
                        final long count = Long.parseLong( value );
                        func.setUpdateCount( transaction, count );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidIntegerPropertyValue, UPDATE_COUNT );
                    }

                    break;
                case USES_DISTINCT_ROWS:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        func.setUsesDistinctRows( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, USES_DISTINCT_ROWS );
                    }

                    break;
                case UUID:
                    func.setUuid( getTransaction(), value );
                    break;
                case VAR_ARGS:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        func.setVarArgs( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, VAR_ARGS );
                    }

                    break;
                default:
                    errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidPropertyName, name, PushdownFunction.class.getSimpleName() );
                    break;
            }

            if ( StringUtils.isBlank( errorMsg ) ) {
                result = new CommandResultImpl( I18n.bind( WorkspaceCommandsI18n.setPropertySuccess, name ) );
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
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( PushdownFunctionCommandsI18n.setPushdownFunctionPropertyHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( PushdownFunctionCommandsI18n.setPushdownFunctionPropertyExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( PushdownFunctionCommandsI18n.setPushdownFunctionPropertyUsage ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion( final String lastArgument,
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
        }

        if ( ( args.size() == 1 ) ) {
            String theArg = getArguments().get(0);
            if( AGGREGATE.equals(theArg) || ALLOWS_DISTINCT.equals(theArg) || ALLOWS_ORDERBY.equals(theArg) || ANALYTIC.equals(theArg)
                || DECOMPOSABLE.equals(theArg) || NULL_ON_NULL.equals(theArg) || USES_DISTINCT_ROWS.equals(theArg) || VAR_ARGS.equals(theArg)) {
                updateCandidatesForBooleanProperty( lastArgument, candidates );
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
        }
        return TabCompletionModifier.AUTO;
    }
}
