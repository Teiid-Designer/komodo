/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.datarole;

import java.util.List;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.relational.vdb.DataRole;
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
 * A shell command to set DataRole properties
 */
public final class SetDataRolePropertyCommand extends DataRoleShellCommand {

    static final String NAME = SetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetDataRolePropertyCommand( final WorkspaceStatus status ) {
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

            final DataRole dataRole = getDataRole();
            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            if ( ALLOWED_CREATE_TEMPORARY_TABLES.equals( name ) ) {
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    dataRole.setAllowCreateTempTables( transaction, Boolean.parseBoolean( value ) );
                } else {
                    errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOWED_CREATE_TEMPORARY_TABLES );
                }
            } else if ( ANY_AUTHENTICATED.equals( name ) ) {
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    dataRole.setAnyAuthenticated( transaction, Boolean.parseBoolean( value ) );
                } else {
                    I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ANY_AUTHENTICATED );
                }
            } else if ( DESCRIPTION.equals( name ) ) {
                dataRole.setDescription( transaction, value );
            } else if ( GRANT_ALL.equals( name ) ) {
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    dataRole.setGrantAll( transaction, Boolean.parseBoolean( value ) );
                } else {
                    errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, GRANT_ALL );
                }
            } else {
                errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidPropertyName, name, DataRole.class.getSimpleName() );
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
        print( indent, I18n.bind( DataRoleCommandsI18n.setDataRolePropertyHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( DataRoleCommandsI18n.setDataRolePropertyExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( DataRoleCommandsI18n.setDataRolePropertyUsage ) );
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
            if(  ALLOWED_CREATE_TEMPORARY_TABLES.equals(theArg) || ANY_AUTHENTICATED.equals(theArg) || GRANT_ALL.equals(theArg) ) {
                updateCandidatesForBooleanProperty( lastArgument, candidates );
            }
        }
        return TabCompletionModifier.AUTO;
    }

}
