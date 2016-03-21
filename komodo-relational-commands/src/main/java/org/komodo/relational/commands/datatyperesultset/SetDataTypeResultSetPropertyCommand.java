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
package org.komodo.relational.commands.datatyperesultset;

import java.util.List;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.SetPropertyCommand;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to set {@link DataTypeResultSet} properties.
 */
public final class SetDataTypeResultSetPropertyCommand extends DataTypeResultSetShellCommand {

    static final String NAME = SetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetDataTypeResultSetPropertyCommand( final WorkspaceStatus status ) {
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

            final DataTypeResultSet rs = getDataTypeResultSet();
            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            switch ( name ) {
                case DATATYPE_NAME: {
                    if ( DataTypeResultSet.Type.BIGDECIMAL.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.BIGDECIMAL );
                    } else if ( DataTypeResultSet.Type.BIGINT.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.BIGINT );
                    } else if ( DataTypeResultSet.Type.BIGINTEGER.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.BIGINTEGER );
                    } else if ( DataTypeResultSet.Type.BLOB.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.BLOB );
                    } else if ( DataTypeResultSet.Type.BOOLEAN.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.BOOLEAN );
                    } else if ( DataTypeResultSet.Type.BYTE.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.BYTE );
                    } else if ( DataTypeResultSet.Type.CHAR.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.CHAR );
                    } else if ( DataTypeResultSet.Type.CLOB.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.CLOB );
                    } else if ( DataTypeResultSet.Type.DATE.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.DATE );
                    } else if ( DataTypeResultSet.Type.DECIMAL.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.DECIMAL );
                    } else if ( DataTypeResultSet.Type.DEFAULT_VALUE.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.DEFAULT_VALUE );
                    } else if ( DataTypeResultSet.Type.DOUBLE.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.DOUBLE );
                    } else if ( DataTypeResultSet.Type.FLOAT.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.FLOAT );
                    } else if ( DataTypeResultSet.Type.INTEGER.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.INTEGER );
                    } else if ( DataTypeResultSet.Type.LONG.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.LONG );
                    } else if ( DataTypeResultSet.Type.OBJECT.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.OBJECT );
                    } else if ( DataTypeResultSet.Type.REAL.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.REAL );
                    } else if ( DataTypeResultSet.Type.SHORT.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.SHORT );
                    } else if ( DataTypeResultSet.Type.SMALLINT.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.SMALLINT );
                    } else if ( DataTypeResultSet.Type.STRING.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.STRING );
                    } else if ( DataTypeResultSet.Type.TIME.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.TIME );
                    } else if ( DataTypeResultSet.Type.TIMESTAMP.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.TIMESTAMP );
                    } else if ( DataTypeResultSet.Type.TINYINT.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.TINYINT );
                    } else if ( DataTypeResultSet.Type.VARBINARY.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.VARBINARY );
                    } else if ( DataTypeResultSet.Type.VARCHAR.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.VARCHAR );
                    } else if ( DataTypeResultSet.Type.XML.name().equals( value ) ) {
                        rs.setType( transaction, DataTypeResultSet.Type.XML );
                    } else {
                        errorMsg = I18n.bind( DataTypeResultSetCommandsI18n.invalidDataTypeName, DATATYPE_NAME );
                    }

                    // check to see if needs to be an array type
                    if ( StringUtils.isBlank( errorMsg ) ) {
                        final String arrayArg = optionalArgument( 2, Boolean.FALSE.toString() );

                        if ( KomodoObjectUtils.TRUE_STRING.equals( arrayArg )
                             || KomodoObjectUtils.FALSE_STRING.equals( arrayArg ) ) {
                            rs.setArray( transaction, Boolean.parseBoolean( arrayArg ) );
                        } else {
                            errorMsg = I18n.bind( DataTypeResultSetCommandsI18n.invalidDataTypeArrayIndicator );
                        }
                    }

                    break;
                }
                case DESCRIPTION:
                    rs.setDescription( transaction, value );
                    break;
                case LENGTH:
                    try {
                        final long length = Long.parseLong( value );
                        rs.setLength( transaction, length );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidIntegerPropertyValue, LENGTH );
                    }

                    break;
                case NAME_IN_SOURCE:
                    rs.setNameInSource( transaction, value );
                    break;
                case NULLABLE:
                    if ( Nullable.NO_NULLS.name().equals( value ) ) {
                        rs.setNullable( transaction, Nullable.NO_NULLS );
                    } else if ( Nullable.NULLABLE.name().equals( value ) ) {
                        rs.setNullable( transaction, Nullable.NULLABLE );
                    } else if ( Nullable.NULLABLE_UNKNOWN.name().equals( value ) ) {
                        rs.setNullable( transaction, Nullable.NULLABLE_UNKNOWN );
                    } else {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidNullablePropertyValue, NULLABLE );
                    }

                    break;
                case PRECISION:
                    try {
                        final long precision = Long.parseLong( value );
                        rs.setPrecision( transaction, precision );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidIntegerPropertyValue, PRECISION );
                    }

                    break;
                case SCALE:
                    try {
                        final long scale = Long.parseLong( value );
                        rs.setScale( transaction, scale );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidIntegerPropertyValue, SCALE );
                    }

                    break;
                case UUID:
                    rs.setUuid( transaction, value );
                    break;
                default:
                    errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidPropertyName, name, DataTypeResultSet.class.getSimpleName() );
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
        return 3;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( DataTypeResultSetCommandsI18n.setDataTypeResultSetPropertyHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( DataTypeResultSetCommandsI18n.setDataTypeResultSetPropertyExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( DataTypeResultSetCommandsI18n.setDataTypeResultSetPropertyUsage ) );
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

        if ( ( args.size() >= 1 ) && DATATYPE_NAME.equals( args.get( 0 ) ) ) {
            if ( args.size() == 1 ) {
                for ( final DataTypeResultSet.Type type : DataTypeResultSet.Type.values() ) {
                    if ( StringUtils.isBlank( lastArgument ) || type.name().startsWith( lastArgument ) ) {
                        candidates.add( type.name() );
                    }
                }
            } else if ( args.size() == 2 ) {
                updateCandidatesForBooleanProperty( lastArgument, candidates );
            }
        }
        return TabCompletionModifier.AUTO;
    }

}
