/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.datatyperesultset;

import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_BOOLEAN_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_INTEGER_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.SET_PROPERTY_ERROR;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.SET_PROPERTY_SUCCESS;
import static org.komodo.relational.commands.model.ModelCommandMessages.General.INVALID_MODEL_TYPE_PROPERTY_VALUE;
import java.util.List;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.UnsetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

/**
 * A shell command to unset DataTypeResultSet properties
 */
public final class SetDataTypeResultSetPropertyCommand extends DataTypeResultSetShellCommand {

    static final String NAME = UnsetPropertyCommand.NAME;

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
            final String name = requiredArgument( 0, getWorkspaceMessage( MISSING_PROPERTY_NAME_VALUE ) );
            final String value = requiredArgument( 1, getWorkspaceMessage( MISSING_PROPERTY_NAME_VALUE ) );

            final DataTypeResultSet rs = getDataTypeResultSet();

            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            switch ( name ) {
                case ARRAY:
                    if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                        rs.setArray( transaction, Boolean.parseBoolean( value ) );
                    } else {
                        errorMsg = getWorkspaceMessage( INVALID_BOOLEAN_PROPERTY_VALUE, ARRAY );
                    }
                    break;
                case LENGTH:
                    try {
                        final long length = Long.parseLong( value );
                        rs.setLength( transaction, length );
                    } catch ( final NumberFormatException e ) {
                        errorMsg = getWorkspaceMessage( INVALID_INTEGER_PROPERTY_VALUE, LENGTH );
                    }
                    break;
                case TYPE:
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
                        errorMsg = getWorkspaceMessage( INVALID_MODEL_TYPE_PROPERTY_VALUE, TYPE );
                    }
                    break;
                default:
                    errorMsg = getWorkspaceMessage( INVALID_PROPERTY_NAME, name, DataTypeResultSet.class.getSimpleName() );
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
            if( ARRAY.equals(theArg) ) {
                candidates.add( Boolean.TRUE.toString() );
                candidates.add( Boolean.FALSE.toString() );
            } else if( TYPE.equals(theArg) ) {
                candidates.add( DataTypeResultSet.Type.BIGDECIMAL.name() );
                candidates.add( DataTypeResultSet.Type.BIGINT.name() );
                candidates.add( DataTypeResultSet.Type.BIGINTEGER.name() );
                candidates.add( DataTypeResultSet.Type.BLOB.name() );
                candidates.add( DataTypeResultSet.Type.BOOLEAN.name() );
                candidates.add( DataTypeResultSet.Type.BYTE.name() );
                candidates.add( DataTypeResultSet.Type.CHAR.name() );
                candidates.add( DataTypeResultSet.Type.CLOB.name() );
                candidates.add( DataTypeResultSet.Type.DATE.name() );
                candidates.add( DataTypeResultSet.Type.DECIMAL.name() );
                candidates.add( DataTypeResultSet.Type.DEFAULT_VALUE.name() );
                candidates.add( DataTypeResultSet.Type.DOUBLE.name() );
                candidates.add( DataTypeResultSet.Type.FLOAT.name() );
                candidates.add( DataTypeResultSet.Type.INTEGER.name() );
                candidates.add( DataTypeResultSet.Type.LONG.name() );
                candidates.add( DataTypeResultSet.Type.OBJECT.name() );
                candidates.add( DataTypeResultSet.Type.REAL.name() );
                candidates.add( DataTypeResultSet.Type.SHORT.name() );
                candidates.add( DataTypeResultSet.Type.SMALLINT.name() );
                candidates.add( DataTypeResultSet.Type.STRING.name() );
                candidates.add( DataTypeResultSet.Type.TIME.name() );
                candidates.add( DataTypeResultSet.Type.TIMESTAMP.name() );
                candidates.add( DataTypeResultSet.Type.TINYINT.name() );
                candidates.add( DataTypeResultSet.Type.VARBINARY.name() );
                candidates.add( DataTypeResultSet.Type.VARCHAR.name() );
                candidates.add( DataTypeResultSet.Type.XML.name() );
            }

            return 0;
        }

        // no tab completion
        return -1;
    }

}
