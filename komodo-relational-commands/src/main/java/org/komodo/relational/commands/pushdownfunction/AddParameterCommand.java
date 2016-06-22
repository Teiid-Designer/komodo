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

import org.komodo.relational.commands.RelationalCommandsI18n;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to add a Parameter to a PushdownFunction.
 */
public final class AddParameterCommand extends PushdownFunctionShellCommand {

    static final String NAME = "add-parameter"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public AddParameterCommand( final WorkspaceStatus status ) {
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
            final String paramName = requiredArgument( 0, I18n.bind( PushdownFunctionCommandsI18n.missingParameterName ) );

            final PushdownFunction func = getPushdownFunction();
            
            // Do not allow add if object of type with this name already exists
            Parameter[] params = func.getParameters(getTransaction(), paramName);
            if(params.length>0) {
                return new CommandResultImpl( false, I18n.bind( RelationalCommandsI18n.cannotAddChildAlreadyExistsError, paramName, Parameter.class.getSimpleName() ), null );
            }
            
            func.addParameter( getTransaction(), paramName );

            result = new CommandResultImpl( I18n.bind( PushdownFunctionCommandsI18n.parameterAdded, paramName ) );
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
        print( indent, I18n.bind( PushdownFunctionCommandsI18n.addParameterHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( PushdownFunctionCommandsI18n.addParameterExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( PushdownFunctionCommandsI18n.addParameterUsage ) );
    }

}
