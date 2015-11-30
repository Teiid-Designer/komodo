/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.shell.commands;

import java.util.List;

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A {@link ShellCommand command} shows the properties and children of a {@link KomodoObject}.
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * show-summary
 * </code>
 */
public class ShowSummaryCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "show-summary"; //$NON-NLS-1$

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public ShowSummaryCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
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
            // Validate the location Path if supplied
            String pathArg = optionalArgument(0);
            if(!StringUtils.isEmpty(pathArg)) {
                String validationMsg = validatePath(pathArg);
                if(!validationMsg.equals(CompletionConstants.OK)) {
                    return new CommandResultImpl(false, validationMsg, null);
                }
            }

            WorkspaceStatus wsStatus = getWorkspaceStatus();
            KomodoObject theContext = getContext();
            if(pathArg!=null) {
                theContext = wsStatus.getContextForDisplayPath( pathArg );
            }

            ShellCommand showChildrenCommand = null;

            if ( isShowChildrenCommandIsValid() ) {
                showChildrenCommand = getShowChildrenCommand();
                showChildrenCommand.setWriter( getWriter() );

                if ( theContext != null ) {
                    showChildrenCommand.setArguments( getArguments() );
                }
            }

            ShellCommand showPropertiesCommand = null;

            if ( isShowPropertiesCommandIsValid() ) {
                showPropertiesCommand = getShowPropertiesCommand();
                showPropertiesCommand.setWriter( getWriter() );

                if ( theContext != null ) {
                    showPropertiesCommand.setArguments( getArguments() );
                }
            }

            if ( showPropertiesCommand != null ) {
                final CommandResult result = showPropertiesCommand.execute();

                if ( !result.isOk() ) {
                    return result;
                }

                print();
            }

            if ( showChildrenCommand != null ) {
                return showChildrenCommand.execute();
            }

            return CommandResult.FAIL;
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
        return 1;
    }

    private ShellCommand getShowChildrenCommand() throws Exception {
        return getWorkspaceStatus().getCommand( ShowChildrenCommand.NAME );
    }

    private ShellCommand getShowPropertiesCommand() throws Exception {
        return getWorkspaceStatus().getCommand( ShowPropertiesCommand.NAME );
    }

    private boolean isShowChildrenCommandIsValid() throws Exception {
        return ShowChildrenCommand.NAME.equals( getShowChildrenCommand().getName() );
    }

    private boolean isShowPropertiesCommandIsValid() throws Exception {
        return ShowPropertiesCommand.NAME.equals( getShowPropertiesCommand().getName() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ShellI18n.showSummaryHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ShellI18n.showSummaryExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ShellI18n.showSummaryUsage ) );
    }

    /**
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {

        if (getArguments().isEmpty()) {
            // The arg is expected to be a path
            updateTabCompleteCandidatesForPath(candidates, getContext(), true, lastArgument);

            // Do not put space after it - may want to append more to the path
            return TabCompletionModifier.NO_APPEND_SEPARATOR;
        }

        return TabCompletionModifier.AUTO;
    }

}
