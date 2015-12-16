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
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to show all the {@link VirtualProcedure virtual procedures} of a {@link Model model}.
 */
public final class ShowVirtualProceduresCommand extends ModelShellCommand {

    static final String NAME = "show-virtual-procedures"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ShowVirtualProceduresCommand( final WorkspaceStatus status ) {
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
            final Procedure[] procedures = model.getProcedures( getTransaction(), namePatterns );

            if ( procedures.length == 0 ) {
                if ( hasPatterns ) {
                    print( MESSAGE_INDENT,
                           I18n.bind( ModelCommandsI18n.noMatchingVirtualProcedures, model.getName( getTransaction() ) ) );
                } else {
                    print( MESSAGE_INDENT,
                           I18n.bind( ModelCommandsI18n.noVirtualProcedures, model.getName( getTransaction() ) ) );
                }
            } else {
                final List< Procedure > virtualProcedures = new ArrayList< >( procedures.length );

                for ( final Procedure procedure : procedures ) {
                    if ( VirtualProcedure.RESOLVER.resolvable( getTransaction(), procedure ) ) {
                        virtualProcedures.add( procedure );
                    }
                }

                if ( virtualProcedures.isEmpty() ) {
                    if ( hasPatterns ) {
                        print( MESSAGE_INDENT,
                               I18n.bind( ModelCommandsI18n.noMatchingVirtualProcedures, model.getName( getTransaction() ) ) );
                    } else {
                        print( MESSAGE_INDENT,
                               I18n.bind( ModelCommandsI18n.noVirtualProcedures, model.getName( getTransaction() ) ) );
                    }
                } else {
                    if ( hasPatterns ) {
                        print( MESSAGE_INDENT,
                               I18n.bind( ModelCommandsI18n.matchedVirtualProceduresHeader, model.getName( getTransaction() ) ) );
                    } else {
                        print( MESSAGE_INDENT,
                               I18n.bind( ModelCommandsI18n.virtualProceduresHeader, model.getName( getTransaction() ) ) );
                    }

                    final int indent = (MESSAGE_INDENT * 2);

                    for ( final Procedure virtProc : virtualProcedures ) {
                        print( indent,
                               I18n.bind( WorkspaceCommandsI18n.printRelationalObject,
                                                    virtProc.getName( getTransaction() ),
                                                    getWorkspaceStatus().getLabelProvider().getTypeDisplay(getTransaction(), virtProc) ) );
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
        print( indent, I18n.bind( ModelCommandsI18n.showVirtualProceduresHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ModelCommandsI18n.showVirtualProceduresExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ModelCommandsI18n.showVirtualProceduresUsage ) );
    }

}
