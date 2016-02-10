/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import org.komodo.relational.RelationalObject;
import org.komodo.relational.model.OptionContainer;
import org.komodo.relational.model.internal.OptionContainerUtils;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.SetPropertyCommand;
import org.komodo.spi.KException;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to set a custom property on a {@link RelationalObject}.
 */
public final class SetCustomOptionCommand extends RelationalShellCommand {

    static final String NAME = "set-custom-option"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public SetCustomOptionCommand( final WorkspaceStatus status ) {
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
            final String name = requiredArgument( 0, I18n.bind( RelationalCommandsI18n.missingOptionNameValue ) );
            final String value = requiredArgument( 1, I18n.bind( RelationalCommandsI18n.missingOptionNameValue ) );
            if(optionContainer.isStandardOption(name)){
            	 throw new KException(I18n.bind( RelationalCommandsI18n.useSetPropertyCommand, SetPropertyCommand.NAME ));
            }
            OptionContainerUtils.setOption(getTransaction(),optionContainer,name,value);
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
        return 2;
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
        print( indent, I18n.bind( RelationalCommandsI18n.setCustomOptionHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( RelationalCommandsI18n.setCustomOptionExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( RelationalCommandsI18n.setCustomOptionUsage ) );
    }


}
