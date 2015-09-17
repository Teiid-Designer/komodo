/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.column;

import static org.komodo.relational.commands.column.ColumnCommandMessages.General.INVALID_NULLABLE_PROPERTY_VALUE;
import static org.komodo.relational.commands.column.ColumnCommandMessages.General.INVALID_SEARCHABLE_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_BOOLEAN_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_INTEGER_PROPERTY_VALUE;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.INVALID_PROPERTY_NAME;
import static org.komodo.relational.commands.WorkspaceCommandMessages.General.MISSING_PROPERTY_NAME_VALUE;
import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.Arrays;
import java.util.List;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Column.Searchable;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.core.SetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A shell command to set Column properties
 */
public final class SetColumnPropertyCommand extends ColumnShellCommand {

    static final String NAME = "set-column-property"; //$NON-NLS-1$

    private static final String AUTO_INCREMENTED = "auto-incremented"; //$NON-NLS-1$
    private static final String CASE_SENSITIVE = "case-sensitive"; //$NON-NLS-1$
    private static final String CHAR_OCTET_LENGTH = "char-octet-lenght"; //$NON-NLS-1$
    private static final String COLLATION_NAME = "collation-name"; //$NON-NLS-1$
    private static final String CURRENCY = "currency"; //$NON-NLS-1$
    private static final String DATATYPE_NAME = "datatype-name"; //$NON-NLS-1$
    private static final String DEFAULT_VALUE = "default-value"; //$NON-NLS-1$
    private static final String DESCRIPTION = "description"; //$NON-NLS-1$
    private static final String DISTINCT_VALUES = "distinct-values"; //$NON-NLS-1$
    private static final String FIXED_LENGTH = "fixed-length"; //$NON-NLS-1$
    private static final String LENGTH = "length"; //$NON-NLS-1$
    private static final String MAX_VALUE = "max-value"; //$NON-NLS-1$
    private static final String MIN_VALUE = "min-value"; //$NON-NLS-1$
    private static final String NAME_IN_SOURCE = "name-in-source"; //$NON-NLS-1$
    private static final String NATIVE_TYPE = "native-type"; //$NON-NLS-1$
    private static final String NULLABLE = "nullable"; //$NON-NLS-1$
    private static final String NULL_VALUE_COUNT = "null-value-count"; //$NON-NLS-1$
    private static final String PRECISION = "precision"; //$NON-NLS-1$
    private static final String RADIX = "radix"; //$NON-NLS-1$
    private static final String SCALE = "scale"; //$NON-NLS-1$
    private static final String SEARCHABLE = "searchable"; //$NON-NLS-1$
    private static final String SELECTABLE = "selectable"; //$NON-NLS-1$
    private static final String SIGNED = "signed"; //$NON-NLS-1$
    private static final String UPDATABLE = "updatable"; //$NON-NLS-1$
    private static final String UUID = "uuid"; //$NON-NLS-1$

    private static final List< String > ALL_PROPS = Arrays.asList( new String[] { AUTO_INCREMENTED, CASE_SENSITIVE, CHAR_OCTET_LENGTH, COLLATION_NAME,
                                                                                  CURRENCY, DATATYPE_NAME, DEFAULT_VALUE, DESCRIPTION,
                                                                                  DISTINCT_VALUES, FIXED_LENGTH, LENGTH,
                                                                                  MAX_VALUE, MIN_VALUE, NAME_IN_SOURCE, NATIVE_TYPE,
                                                                                  NULLABLE, NULL_VALUE_COUNT, PRECISION,
                                                                                  RADIX, SCALE, SEARCHABLE, SELECTABLE,
                                                                                  SIGNED, UPDATABLE, UUID } );

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetColumnPropertyCommand( final WorkspaceStatus status ) {
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

        final Column column = getColumn();
        
        final UnitOfWork transaction = getTransaction();
        boolean success = true;

        switch ( name ) {
            case AUTO_INCREMENTED:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    column.setAutoIncremented( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, AUTO_INCREMENTED ) );
                    success = false;
                }

                break;
            case CASE_SENSITIVE:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    column.setCaseSensitive( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, CASE_SENSITIVE ) );
                    success = false;
                }

                break;
            case CHAR_OCTET_LENGTH:
                try {
                    final long octetLength = Long.parseLong( value );
                    column.setCharOctetLength(transaction, octetLength);
                } catch ( final NumberFormatException e ) {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, CHAR_OCTET_LENGTH ) );
                    success = false;
                }

                break;
            case COLLATION_NAME:
                column.setCollationName(transaction, value);
                break;
            case CURRENCY:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    column.setCurrency( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, CURRENCY ) );
                    success = false;
                }

                break;
            case DATATYPE_NAME:
                column.setDatatypeName(transaction, value);
                break;
            case DEFAULT_VALUE:
                column.setDefaultValue(transaction, value);
                break;
            case DESCRIPTION:
                column.setDescription(transaction, value);
                break;
            case DISTINCT_VALUES:
                try {
                    final long nValues = Long.parseLong( value );
                    column.setDistinctValues(transaction, nValues);
                } catch ( final NumberFormatException e ) {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, DISTINCT_VALUES ) );
                    success = false;
                }

                break;
            case FIXED_LENGTH:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    column.setFixedLength( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, FIXED_LENGTH ) );
                    success = false;
                }

                break;
            case LENGTH:
                try {
                    final long length = Long.parseLong( value );
                    column.setLength(transaction, length);
                } catch ( final NumberFormatException e ) {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, LENGTH ) );
                    success = false;
                }

                break;
            case MAX_VALUE:
                column.setMaxValue(transaction, value);
                break;
            case MIN_VALUE:
                column.setMinValue(transaction, value);
                break;
            case NAME_IN_SOURCE:
                column.setNameInSource(transaction, value);
                break;
            case NATIVE_TYPE:
                column.setNativeType(transaction, value);
                break;
            case NULLABLE:
                if ( Nullable.NO_NULLS.name().equals( value ) ) {
                    column.setNullable( transaction, Nullable.NO_NULLS );
                } else if ( Nullable.NULLABLE.name().equals( value ) ) {
                    column.setNullable( transaction, Nullable.NULLABLE );
                } else if ( Nullable.NULLABLE_UNKNOWN.name().equals( value ) ) {
                    column.setNullable( transaction, Nullable.NULLABLE_UNKNOWN );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_NULLABLE_PROPERTY_VALUE, NULLABLE ) );
                    success = false;
                }
                break;
            case NULL_VALUE_COUNT:
                try {
                    final long count = Long.parseLong( value );
                    column.setNullValueCount(transaction, count);
                } catch ( final NumberFormatException e ) {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, NULL_VALUE_COUNT ) );
                    success = false;
                }

                break;
            case PRECISION:
                try {
                    final long precision = Long.parseLong( value );
                    column.setPrecision(transaction, precision);
                } catch ( final NumberFormatException e ) {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, PRECISION ) );
                    success = false;
                }

                break;
            case RADIX:
                try {
                    final long radix = Long.parseLong( value );
                    column.setRadix(transaction, radix);
                } catch ( final NumberFormatException e ) {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, RADIX ) );
                    success = false;
                }
                break;
            case SCALE:
                try {
                    final long scale = Long.parseLong( value );
                    column.setScale(transaction, scale);
                } catch ( final NumberFormatException e ) {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_INTEGER_PROPERTY_VALUE, SCALE ) );
                    success = false;
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
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_SEARCHABLE_PROPERTY_VALUE, SEARCHABLE ) );
                    success = false;
                }
                break;
            case SELECTABLE:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    column.setSelectable( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, SELECTABLE ) );
                    success = false;
                }

                break;
            case SIGNED:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    column.setSigned( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, SIGNED ) );
                    success = false;
                }

                break;
            case UPDATABLE:
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    column.setUpdatable( transaction, Boolean.parseBoolean( value ) );
                } else {
                    print( MESSAGE_INDENT, getWorkspaceMessage(INVALID_BOOLEAN_PROPERTY_VALUE, UPDATABLE ) );
                    success = false;
                }

                break;
            case UUID:
                column.setUuid(transaction, value);
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
            if( AUTO_INCREMENTED.equals(theArg) || CASE_SENSITIVE.equals(theArg) || CURRENCY.equals(theArg)
             || FIXED_LENGTH.equals(theArg) || SELECTABLE.equals(theArg) || UPDATABLE.equals(theArg)) {
                candidates.add( Boolean.TRUE.toString() );
                candidates.add( Boolean.FALSE.toString() );
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

            return 0;
        }
        
        // no tab completion
        return -1;
    }

}
