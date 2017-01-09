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
package org.komodo.relational.commands;

import java.util.List;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.model.OptionContainer;
import org.komodo.relational.model.StatementOption;
import org.komodo.relational.model.internal.OptionContainerUtils;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.UnsetPropertyCommand;
import org.komodo.spi.KException;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to set a custom property on a {@link RelationalObject}.
 */
public final class UnsetCustomOptionCommand extends RelationalShellCommand {

    static final String NAME = "unset-custom-option"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public UnsetCustomOptionCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        CommandResult result = null;
        //Cast is save since this command can be invoked only for objects implementing OptionContainer
        OptionContainer optionContainer=(OptionContainer)getContext();

        try {
            final String name = requiredArgument( 0, I18n.bind( RelationalCommandsI18n.unsetMissingOptionName ) );
            if(optionContainer.isStandardOption(name)){
            	 throw new KException(I18n.bind( RelationalCommandsI18n.useSetPropertyCommand, UnsetPropertyCommand.NAME ));
            }
            OptionContainerUtils.removeOption(getTransaction(), optionContainer, name);
            result = new CommandResultImpl( I18n.bind( RelationalCommandsI18n.setCustomOptionSuccess, name ) );
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
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return (getContext() instanceof OptionContainer);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( RelationalCommandsI18n.unsetCustomOptionHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( RelationalCommandsI18n.unsetCustomOptionExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( RelationalCommandsI18n.unsetCustomOptionUsage ) );
    }

    /**
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion(final String lastArgument,
                                               final List<CharSequence> candidates) throws Exception {
        String last=""; //$NON-NLS-1$
        if(!StringUtils.isEmpty(lastArgument)){
            last=lastArgument;
        }
        if (getArguments().size() == 0) {
            if (!(getContext() instanceof OptionContainer)) {
                return TabCompletionModifier.NO_AUTOCOMPLETION;
            }
            StatementOption[] options = ((OptionContainer)getContext()).getCustomOptions(getTransaction());
            for (StatementOption option : options) {
                String optionName = option.getName(getTransaction());
                if (optionName.startsWith(last)) {
                    candidates.add(optionName);
                }
            }
        }
        return TabCompletionModifier.AUTO;
    }


}
