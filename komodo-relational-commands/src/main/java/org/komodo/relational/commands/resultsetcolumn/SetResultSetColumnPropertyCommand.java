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
package org.komodo.relational.commands.resultsetcolumn;

import java.util.List;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ResultSetColumn;
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
 * A shell command to set Column properties
 */
public final class SetResultSetColumnPropertyCommand extends ResultSetColumnShellCommand {

    static final String NAME = SetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetResultSetColumnPropertyCommand( final WorkspaceStatus status ) {
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

            final ResultSetColumn column = getResultSetColumn();
            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            switch ( name ) {
                case DATATYPE_NAME:
                    column.setDatatypeName( transaction, value );
                    break;
                case DEFAULT_VALUE:
                    column.setDefaultValue( transaction, value );
                    break;
                case DESCRIPTION:
                    column.setDescription( transaction, value );
                    break;
                case LENGTH:
                    try {
                        final long length = Long.parseLong( value );
                        column.setLength( transaction, length );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidIntegerPropertyValue, LENGTH );
                    }

                    break;
                case NAME_IN_SOURCE:
                    column.setNameInSource( transaction, value );
                    break;
                case NULLABLE:
                    if ( Nullable.NO_NULLS.name().equals( value ) ) {
                        column.setNullable( transaction, Nullable.NO_NULLS );
                    } else if ( Nullable.NULLABLE.name().equals( value ) ) {
                        column.setNullable( transaction, Nullable.NULLABLE );
                    } else if ( Nullable.NULLABLE_UNKNOWN.name().equals( value ) ) {
                        column.setNullable( transaction, Nullable.NULLABLE_UNKNOWN );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidNullablePropertyValue, NULLABLE );
                    }

                    break;
                case PRECISION:
                    try {
                        final long precision = Long.parseLong( value );
                        column.setPrecision( transaction, precision );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidIntegerPropertyValue, PRECISION );
                    }

                    break;
                case SCALE:
                    try {
                        final long scale = Long.parseLong( value );
                        column.setScale( transaction, scale );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidIntegerPropertyValue, SCALE );
                    }

                    break;
                case UUID:
                    column.setUuid( transaction, value );
                    break;
                default:
                    errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidPropertyName, name, Column.class.getSimpleName() );
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
        print( indent, I18n.bind( ResultSetColumnCommandsI18n.setResultSetColumnPropertyHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ResultSetColumnCommandsI18n.setResultSetColumnPropertyExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ResultSetColumnCommandsI18n.setResultSetColumnPropertyUsage ) );
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
            if( NULLABLE.equals(theArg) ) {
                candidates.add( Nullable.NO_NULLS.name() );
                candidates.add( Nullable.NULLABLE.name() );
                candidates.add( Nullable.NULLABLE_UNKNOWN.name() );
            }
        }
        return TabCompletionModifier.AUTO;
    }

}
