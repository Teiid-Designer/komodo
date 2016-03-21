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
package org.komodo.relational.commands.column;

import java.util.List;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Column.Searchable;
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
public final class SetColumnPropertyCommand extends ColumnShellCommand {

    static final String NAME = SetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetColumnPropertyCommand( final WorkspaceStatus status ) {
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

            final Column column = getColumn();
            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            switch ( name ) {
                case AUTO_INCREMENTED:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        column.setAutoIncremented( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, AUTO_INCREMENTED );
                    }

                    break;
                case CASE_SENSITIVE:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        column.setCaseSensitive( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, CASE_SENSITIVE );
                    }

                    break;
                case CHAR_OCTET_LENGTH:
                    try {
                        final long octetLength = Long.parseLong( value );
                        column.setCharOctetLength( transaction, octetLength );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidIntegerPropertyValue, CHAR_OCTET_LENGTH );
                    }

                    break;
                case COLLATION_NAME:
                    column.setCollationName( transaction, value );
                    break;
                case CURRENCY:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        column.setCurrency( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, CURRENCY );
                    }

                    break;
                case DATATYPE_NAME:
                    column.setDatatypeName( transaction, value );
                    break;
                case DEFAULT_VALUE:
                    column.setDefaultValue( transaction, value );
                    break;
                case DESCRIPTION:
                    column.setDescription( transaction, value );
                    break;
                case DISTINCT_VALUES:
                    try {
                        final long nValues = Long.parseLong( value );
                        column.setDistinctValues( transaction, nValues );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidIntegerPropertyValue, DISTINCT_VALUES );
                    }

                    break;
                case FIXED_LENGTH:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        column.setFixedLength( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, FIXED_LENGTH );
                    }

                    break;
                case LENGTH:
                    try {
                        final long length = Long.parseLong( value );
                        column.setLength( transaction, length );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidIntegerPropertyValue, LENGTH );
                    }

                    break;
                case MAX_VALUE:
                    column.setMaxValue( transaction, value );
                    break;
                case MIN_VALUE:
                    column.setMinValue( transaction, value );
                    break;
                case NAME_IN_SOURCE:
                    column.setNameInSource( transaction, value );
                    break;
                case NATIVE_TYPE:
                    column.setNativeType( transaction, value );
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
                case NULL_VALUE_COUNT:
                    try {
                        final long count = Long.parseLong( value );
                        column.setNullValueCount( transaction, count );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidIntegerPropertyValue, NULL_VALUE_COUNT );
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
                case RADIX:
                    try {
                        final long radix = Long.parseLong( value );
                        column.setRadix( transaction, radix );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidIntegerPropertyValue, RADIX );
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
                case SEARCHABLE:
                    if ( Searchable.ALL_EXCEPT_LIKE.name().equals( value ) ) {
                        column.setSearchable( transaction, Searchable.ALL_EXCEPT_LIKE );
                    } else if ( Searchable.LIKE_ONLY.name().equals( value ) ) {
                        column.setSearchable( transaction, Searchable.LIKE_ONLY );
                    } else if ( Searchable.SEARCHABLE.name().equals( value ) ) {
                        column.setSearchable( transaction, Searchable.SEARCHABLE );
                    } else if ( Searchable.UNSEARCHABLE.name().equals( value ) ) {
                        column.setSearchable( transaction, Searchable.UNSEARCHABLE );
                    } else {
                        errorMsg = I18n.bind( ColumnCommandsI18n.invalidSearchablePropertyValue, SEARCHABLE );
                    }

                    break;
                case SELECTABLE:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        column.setSelectable( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, SELECTABLE );
                    }

                    break;
                case SIGNED:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        column.setSigned( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, SIGNED );
                    }

                    break;
                case UPDATABLE:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        column.setUpdatable( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, UPDATABLE );
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
        print( indent, I18n.bind( ColumnCommandsI18n.setColumnPropertyHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ColumnCommandsI18n.setColumnPropertyExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ColumnCommandsI18n.setColumnPropertyUsage ) );
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
            if( AUTO_INCREMENTED.equals(theArg) || CASE_SENSITIVE.equals(theArg) || CURRENCY.equals(theArg)
             || FIXED_LENGTH.equals(theArg) || SELECTABLE.equals(theArg) || UPDATABLE.equals(theArg)) {
                updateCandidatesForBooleanProperty( lastArgument, candidates );
            } else if( NULLABLE.equals(theArg) ) {
                candidates.add( Nullable.NO_NULLS.name() );
                candidates.add( Nullable.NULLABLE.name() );
                candidates.add( Nullable.NULLABLE_UNKNOWN.name() );
            } else if( SEARCHABLE.equals(theArg)) {
                candidates.add( Searchable.ALL_EXCEPT_LIKE.name() );
                candidates.add( Searchable.LIKE_ONLY.name() );
                candidates.add( Searchable.SEARCHABLE.name() );
                candidates.add( Searchable.UNSEARCHABLE.name() );
            }
        }
        return TabCompletionModifier.AUTO;
    }

}
