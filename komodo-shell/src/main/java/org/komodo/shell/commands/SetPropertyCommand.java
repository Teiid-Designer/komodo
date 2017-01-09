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
package org.komodo.shell.commands;

import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.PropertyDescriptor.Type;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A {@link ShellCommand command} that sets a property value on a {@link KomodoObject}.
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * set-property &lt;prop-name&gt; &lt;prop-value&gt;
 * </code>
 */
public class SetPropertyCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "set-property"; //$NON-NLS-1$

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public SetPropertyCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        // Not valid in root, workspace, library or environment
        if( KomodoObjectUtils.isRoot(getContext()) || KomodoObjectUtils.isRootChild(getTransaction(), getContext()) ) {
            return false;
        }
        return true;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            // property name and value are required
            final String propNameArg = requiredArgument( 0, I18n.bind( ShellI18n.invalidArgMsgPropertyName ) );
            final String propValueArg = requiredArgument( 1, I18n.bind( ShellI18n.invalidArgMsgPropertyValue ) );
            final KomodoObject context = getContext();

            // Validate the property value
            if ( !KomodoObjectUtils.isValidPropertyValue( getWorkspaceStatus(), propNameArg, propValueArg, context ) ) {
                return new CommandResultImpl( false,
                                              I18n.bind( ShellI18n.invalidPropValue, propValueArg ),
                                              null );
            }

            // Set the property
            setProperty( context, propNameArg, propValueArg );
            return new CommandResultImpl( I18n.bind( ShellI18n.propertySet, propNameArg ) );
        } catch ( final Exception e ) {
            return new CommandResultImpl( e );
        }
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
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ShellI18n.setPropertyHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ShellI18n.setPropertyExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ShellI18n.setPropertyUsage ) );
    }

    private void setProperty( final KomodoObject context,
                              final String name,
                              final String propValue ) throws Exception {
        final String propertyName = isShowingPropertyNamePrefixes() ? name : KomodoObjectUtils.attachPrefix( getWorkspaceStatus(),context, name );

        if ( !StringUtils.isBlank( propValue )
             && KomodoObjectUtils.isMultiValuedProperty( getWorkspaceStatus(), context, propertyName ) ) {
            final String[] values = KomodoObjectUtils.parseMultiValues( propValue );
            context.setProperty( getTransaction(), propertyName, ( Object[] )values );
        } else {
            context.setProperty( getTransaction(), propertyName, propValue );
        }
    }

    /**
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        if ( getArguments().size() == 0 ) {
            updateTabCompleteCandidatesForProperty( candidates, getContext(), lastArgument );
        } else if (getArguments().size() == 1) {
            final KomodoObject context = getContext();
            String propArg = getArguments().get(0);
            final String propName = isShowingPropertyNamePrefixes() ? propArg
                                                                    : KomodoObjectUtils.attachPrefix( getWorkspaceStatus(),
                                                                                                      context,
                                                                                                      propArg );

            if ( KomodoObjectUtils.isMultiValuedProperty( getWorkspaceStatus(), context, propName ) ) {
                if ( context.hasProperty( getTransaction(), propName ) ) {
                    // concat current multi-values as a string
                    final String value = KomodoObjectUtils.concatMultiValues( getWorkspaceStatus(), context, propName );
                    candidates.add( value );
                }
            } else {
                provideCandidates( context, propName, lastArgument, candidates );
            }
        }

        return TabCompletionModifier.AUTO;
    }

    private void provideCandidates( final KomodoObject context,
                                    final String name,
                                    final String lastArgument,
                                    final List< CharSequence > candidates ) throws Exception {
        final String propertyName = isShowingPropertyNamePrefixes() ? name : KomodoObjectUtils.attachPrefix( getWorkspaceStatus(),context, name );
        final PropertyDescriptor descriptor = context.getPropertyDescriptor( getTransaction(), propertyName );

        if ( descriptor == null ) {
            return;
        }

        String[] possibleValues = StringConstants.EMPTY_ARRAY;

        if ( Type.BOOLEAN == descriptor.getType() ) {
            possibleValues = new String[] { Boolean.TRUE.toString(), Boolean.FALSE.toString() };
        } else {
            final Object[] defaultValues = descriptor.getDefaultValues();

            if ( defaultValues.length != 0 ) {
                possibleValues = new String[ defaultValues.length ];
                int i = 0;

                for ( final Object defaultValue : defaultValues ) {
                    possibleValues[ i++ ] = defaultValue.toString();
                }
            }
        }

        if ( possibleValues.length != 0 ) {
            final boolean hasLastArgument = !StringUtils.isBlank( lastArgument );

            for ( final String value : possibleValues ) {
                if ( ( hasLastArgument && value.startsWith( lastArgument ) ) || !hasLastArgument ) {
                    candidates.add( value );
                }
            }
        }
    }

}
