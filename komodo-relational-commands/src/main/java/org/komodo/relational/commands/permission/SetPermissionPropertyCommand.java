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
package org.komodo.relational.commands.permission;

import java.util.List;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.relational.vdb.Permission;
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
 * A shell command to set Permission properties
 */
public final class SetPermissionPropertyCommand extends PermissionShellCommand {

    static final String NAME = SetPropertyCommand.NAME;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetPermissionPropertyCommand( final WorkspaceStatus status ) {
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

            final Permission permission = getPermission();

            final UnitOfWork transaction = getTransaction();
            String errorMsg = null;

            if ( ALLOW_ALTER.equals( name ) ) {
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    permission.setAllowAlter( transaction, Boolean.parseBoolean( value ) );
                } else {
                    errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOW_ALTER );
                }
            } else if ( ALLOW_CREATE.equals( name ) ) {
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    permission.setAllowCreate( transaction, Boolean.parseBoolean( value ) );
                } else {
                    errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOW_CREATE );
                }
            } else if ( ALLOW_DELETE.equals( name ) ) {
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    permission.setAllowDelete( transaction, Boolean.parseBoolean( value ) );
                } else {
                    errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOW_DELETE );
                }
            } else if ( ALLOW_EXECUTE.equals( name ) ) {
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    permission.setAllowExecute( transaction, Boolean.parseBoolean( value ) );
                } else {
                    errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOW_EXECUTE );
                }
            } else if ( ALLOW_LANGUAGE.equals( name ) ) {
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    permission.setAllowLanguage( transaction, Boolean.parseBoolean( value ) );
                } else {
                    errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOW_LANGUAGE );
                }
            } else if ( ALLOW_READ.equals( name ) ) {
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    permission.setAllowRead( transaction, Boolean.parseBoolean( value ) );
                } else {
                    errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOW_READ );
                }
            } else if ( ALLOW_UPDATE.equals( name ) ) {
                if ( Boolean.TRUE.toString().equals( value ) || Boolean.FALSE.toString().equals( value ) ) {
                    permission.setAllowUpdate( transaction, Boolean.parseBoolean( value ) );
                } else {
                    errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidBooleanPropertyValue, ALLOW_UPDATE );
                }
            } else {
                errorMsg = I18n.bind( WorkspaceCommandsI18n.invalidPropertyName, name, Permission.class.getSimpleName() );
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
        print( indent, I18n.bind( PermissionCommandsI18n.setPermissionPropertyHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( PermissionCommandsI18n.setPermissionPropertyExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( PermissionCommandsI18n.setPermissionPropertyUsage ) );
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
            if( ALLOW_ALTER.equals(theArg) || ALLOW_CREATE.equals(theArg) || ALLOW_DELETE.equals(theArg) || ALLOW_EXECUTE.equals(theArg) ||
                ALLOW_LANGUAGE.equals(theArg) || ALLOW_READ.equals(theArg) || ALLOW_UPDATE.equals(theArg) ) {
                updateCandidatesForBooleanProperty( lastArgument, candidates );
            }
        }
        return TabCompletionModifier.AUTO;
    }

}
