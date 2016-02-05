/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.storedprocedure;

import java.util.List;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.relational.model.SchemaElement;
import org.komodo.relational.model.StoredProcedure;
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
 * A shell command to set StoredProcedrue properties
 */
public final class SetStoredProcedurePropertyCommand extends StoredProcedureShellCommand {

    static final String NAME = SetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetStoredProcedurePropertyCommand( final WorkspaceStatus status ) {
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

            final StoredProcedure proc = getStoredProcedure();
            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            if ( DESCRIPTION.equals( name ) ) {
                proc.setDescription( getTransaction(), value );
            } else if ( NAME_IN_SOURCE.equals( name ) ) {
                proc.setNameInSource( getTransaction(), value );
            } else if ( NATIVE_QUERY.equals( name ) ) {
                proc.setNativeQuery( transaction, value );
            } else if ( NON_PREPARED.equals( name ) ) {
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    proc.setNonPrepared( transaction, Boolean.parseBoolean( value ) );
                } else {
                    errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, NON_PREPARED );
                }
            } else if ( SCHEMA_ELEMENT_TYPE.equals( name ) ) {
                if ( SchemaElement.SchemaElementType.FOREIGN.name().equals( value ) ) {
                    proc.setSchemaElementType( transaction, SchemaElement.SchemaElementType.FOREIGN );
                } else if ( SchemaElement.SchemaElementType.VIRTUAL.name().equals( value ) ) {
                    proc.setSchemaElementType( transaction, SchemaElement.SchemaElementType.VIRTUAL );
                } else {
                    errorMsg = I18n.bind( StoredProcedureCommandsI18n.invalidSchemaElementTypePropertyValue, value );
                }
            } else if ( UPDATE_COUNT.equals( name ) ) {
                try {
                    final long count = Long.parseLong( value );
                    proc.setUpdateCount( transaction, count );
                } catch ( final NumberFormatException e ) {
                    errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidIntegerPropertyValue, UPDATE_COUNT );
                }
            } else if ( UUID.equals( name ) ) {
                proc.setUuid( getTransaction(), value );
            } else {
                errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidPropertyName, name, StoredProcedure.class.getSimpleName() );
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
        print( indent, I18n.bind( StoredProcedureCommandsI18n.setStoredProcedurePropertyHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( StoredProcedureCommandsI18n.setStoredProcedurePropertyExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( StoredProcedureCommandsI18n.setStoredProcedurePropertyUsage ) );
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
            if( NON_PREPARED.equals(theArg) ) {
                updateCandidatesForBooleanProperty( lastArgument, candidates );
            } else if( SCHEMA_ELEMENT_TYPE.equals(theArg)) {
                candidates.add( SchemaElement.SchemaElementType.FOREIGN.name() );
                candidates.add( SchemaElement.SchemaElementType.VIRTUAL.name() );
            }
        }
        return TabCompletionModifier.AUTO;
    }

}
