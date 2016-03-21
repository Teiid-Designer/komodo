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
package org.komodo.relational.commands.pushdownfunction;

import java.util.Arrays;
import java.util.List;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to set the result set for a PushdownFunction.
 */
public final class SetResultSetCommand extends PushdownFunctionShellCommand {

    static final String NAME = "set-result-set"; //$NON-NLS-1$

    protected static final List< String > ALL_TYPES = Arrays.asList( new String[] { DataTypeResultSet.class.getSimpleName(),
                                                                                    TabularResultSet.class.getSimpleName()} );

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetResultSetCommand( final WorkspaceStatus status ) {
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
            final String rsType = requiredArgument( 0, I18n.bind( PushdownFunctionCommandsI18n.missingResultSetType ) );

            final PushdownFunction func = getPushdownFunction();
            if(rsType.equals(DataTypeResultSet.class.getSimpleName())) {
                func.setResultSet( getTransaction(), DataTypeResultSet.class );
            } else if(rsType.equals(TabularResultSet.class.getSimpleName())) {
                func.setResultSet( getTransaction(), TabularResultSet.class );
            } else {
                new CommandResultImpl( false, I18n.bind( PushdownFunctionCommandsI18n.invalidResultSetType, rsType ), null );
            }

            result = new CommandResultImpl( I18n.bind( PushdownFunctionCommandsI18n.resultSetTypeSet, rsType ) );
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
        print( indent, I18n.bind( PushdownFunctionCommandsI18n.setResultSetHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( PushdownFunctionCommandsI18n.setResultSetExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( PushdownFunctionCommandsI18n.setResultSetUsage ) );
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
                candidates.addAll( ALL_TYPES );
            } else {
                for ( final String item : ALL_TYPES ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            }
        }
        return TabCompletionModifier.AUTO;
    }

}
