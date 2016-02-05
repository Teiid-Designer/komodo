/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.column;

import java.util.List;
import org.komodo.relational.RelationalConstants;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.relational.model.Column;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.UnsetPropertyCommand;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to unset {@link Column column} properties.
 */
public final class UnsetColumnPropertyCommand extends ColumnShellCommand {

    static final String NAME = UnsetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public UnsetColumnPropertyCommand( final WorkspaceStatus status ) {
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
            final String name = requiredArgument( 0, I18n.bind( WorkspaceCommandsI18n.unsetMissingPropertyName ) );
            final Column column = getColumn();
            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            if ( AUTO_INCREMENTED.equals( name ) ) {
                column.setAutoIncremented( transaction, Column.DEFAULT_AUTO_INCREMENTED );
            } else if ( CASE_SENSITIVE.equals( name ) ) {
                column.setCaseSensitive( transaction, Column.DEFAULT_CASE_SENSITIVE );
            } else if ( CHAR_OCTET_LENGTH.equals( name ) ) {
                column.setCharOctetLength( transaction, Column.DEFAULT_CHAR_OCTET_LENGTH );
            } else if ( COLLATION_NAME.equals( name ) ) {
                column.setCollationName( transaction, null );
            } else if ( CURRENCY.equals( name ) ) {
                column.setCurrency( transaction, Column.DEFAULT_CURRENCY );
            } else if ( DATATYPE_NAME.equals( name ) ) {
                column.setDatatypeName( transaction, null );
            } else if ( DEFAULT_VALUE.equals( name ) ) {
                column.setDefaultValue( transaction, null );
            } else if ( DESCRIPTION.equals( name ) ) {
                column.setDescription( transaction, null );
            } else if ( DISTINCT_VALUES.equals( name ) ) {
                column.setDistinctValues( transaction, Column.DEFAULT_DISTINCT_VALUES );
            } else if ( FIXED_LENGTH.equals( name ) ) {
                column.setFixedLength( transaction, Column.DEFAULT_FIXED_LENGTH );
            } else if ( LENGTH.equals( name ) ) {
                column.setLength( transaction, RelationalConstants.DEFAULT_LENGTH );
            } else if ( MAX_VALUE.equals( name ) ) {
                column.setMaxValue( transaction, null );
            } else if ( MIN_VALUE.equals( name ) ) {
                column.setMinValue( transaction, null );
            } else if ( NAME_IN_SOURCE.equals( name ) ) {
                column.setNameInSource( transaction, null );
            } else if ( NATIVE_TYPE.equals( name ) ) {
                column.setNativeType( transaction, null );
            } else if ( NULLABLE.equals( name ) ) {
                column.setNullable( transaction, null );
            } else if ( NULL_VALUE_COUNT.equals( name ) ) {
                column.setNullValueCount( transaction, Column.DEFAULT_NULL_VALUE_COUNT );
            } else if ( PRECISION.equals( name ) ) {
                column.setPrecision( transaction, RelationalConstants.DEFAULT_PRECISION );
            } else if ( RADIX.equals( name ) ) {
                column.setRadix( transaction, Column.DEFAULT_RADIX );
            } else if ( SCALE.equals( name ) ) {
                column.setScale( transaction, RelationalConstants.DEFAULT_SCALE );
            } else if ( SEARCHABLE.equals( name ) ) {
                column.setSearchable( transaction, null );
            } else if ( SELECTABLE.equals( name ) ) {
                column.setSelectable( transaction, Column.DEFAULT_SELECTABLE );
            } else if ( SIGNED.equals( name ) ) {
                column.setSigned( transaction, Column.DEFAULT_SIGNED );
            } else if ( UPDATABLE.equals( name ) ) {
                column.setUpdatable( transaction, Column.DEFAULT_UPDATABLE );
            } else if ( UUID.equals( name ) ) {
                column.setUuid( transaction, null );
            } else {
                errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidPropertyName, name, Column.class.getSimpleName() );
            }

            if ( StringUtils.isBlank( errorMsg ) ) {
                result = new CommandResultImpl( I18n.bind( WorkspaceCommandsI18n.unsetPropertySuccess,
                                                           column.getName( transaction ),
                                                           name ) );
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
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ColumnCommandsI18n.unsetColumnPropertyHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ColumnCommandsI18n.unsetColumnPropertyExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ColumnCommandsI18n.unsetColumnPropertyUsage ) );
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
        return TabCompletionModifier.AUTO;
    }

}
