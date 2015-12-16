/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.model;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;

import java.util.ArrayList;
import java.util.List;

import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to show all the {@link UserDefinedFunction user-defined functions} of a {@link Model model}.
 */
public final class ShowUserDefinedFunctionsCommand extends ModelShellCommand {

    static final String NAME = "show-user-defined-functions"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowUserDefinedFunctionsCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            final String[] namePatterns = processOptionalArguments( 0 );
            final boolean hasPatterns = ( namePatterns.length != 0 );
            final Model model = getModel();
            final Function[] functions = model.getFunctions( getTransaction(), namePatterns );

            if ( functions.length == 0 ) {
                if ( hasPatterns ) {
                    print( MESSAGE_INDENT,
                           I18n.bind( ModelCommandsI18n.noMatchingUserDefinedFunctions, model.getName( getTransaction() ) ) );
                } else {
                    print( MESSAGE_INDENT,
                           I18n.bind( ModelCommandsI18n.noUserDefinedFunctions, model.getName( getTransaction() ) ) );
                }
            } else {
                final List< Function > udfs = new ArrayList< >( functions.length );

                for ( final Function function : functions ) {
                    if ( UserDefinedFunction.RESOLVER.resolvable( getTransaction(), function ) ) {
                        udfs.add( function );
                    }
                }

                if ( udfs.isEmpty() ) {
                    if ( hasPatterns ) {
                        print( MESSAGE_INDENT,
                               I18n.bind( ModelCommandsI18n.noMatchingUserDefinedFunctions, model.getName( getTransaction() ) ) );
                    } else {
                        print( MESSAGE_INDENT,
                               I18n.bind( ModelCommandsI18n.noUserDefinedFunctions, model.getName( getTransaction() ) ) );
                    }
                } else {
                    if ( hasPatterns ) {
                        print( MESSAGE_INDENT, I18n.bind( ModelCommandsI18n.matchedUserDefinedFunctionsHeader,
                                                          model.getName( getTransaction() ) ) );
                    } else {
                        print( MESSAGE_INDENT,
                               I18n.bind( ModelCommandsI18n.userDefinedFunctionsHeader, model.getName( getTransaction() ) ) );
                    }

                    final int indent = (MESSAGE_INDENT * 2);

                    for ( final Function function : udfs ) {
                        print( indent,
                               I18n.bind( WorkspaceCommandsI18n.printRelationalObject,
                                                    function.getName( getTransaction() ),
                                                    getWorkspaceStatus().getLabelProvider().getTypeDisplay(getTransaction(), function) ) );
                    }
                }
            }

            return CommandResult.SUCCESS;
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
        return -1;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ModelCommandsI18n.showUserDefinedFunctionsHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ModelCommandsI18n.showUserDefinedFunctionsExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ModelCommandsI18n.showUserDefinedFunctionsUsage ) );
    }

}
