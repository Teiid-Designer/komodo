/*
 * Copyright 2012 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.shell.commands;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A {@link ShellCommand command} that exits the shell program.
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * exit [-f | --force | -s | --save]
 * </code>
 */
public final class ExitCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "exit"; //$NON-NLS-1$

    private static final List< String > FORCE_ARGS = Arrays.asList( new String[] { "-f", "--force" } ); //$NON-NLS-1$ //$NON-NLS-2$
    private static final List< String > SAVE_ARGS = Arrays.asList( new String[] { "-s", "--save" } ); //$NON-NLS-1$ //$NON-NLS-2$
    private static final List< String > VALID_ARGS;

    static {
        VALID_ARGS = new ArrayList<>(FORCE_ARGS.size() + SAVE_ARGS.size());
        VALID_ARGS.addAll( FORCE_ARGS );
        VALID_ARGS.addAll( SAVE_ARGS );
    }

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public ExitCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME, "quit" ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            final String arg = optionalArgument( 0 );
            final boolean hasArg = !StringUtils.isBlank( arg );

            // make sure arg is valid
            if ( hasArg && !VALID_ARGS.contains( arg ) ) {
                final String errorMsg = I18n.bind( ShellI18n.invalidExitArg, arg );
                throw new InvalidCommandArgumentException( 0, errorMsg );
            }

            // max of one argument (this is checked by super)
            assert( optionalArgument( 1 ) == null );

            boolean force = ( hasArg && FORCE_ARGS.contains( arg ) );
            boolean save = ( hasArg && SAVE_ARGS.contains( arg ) );

            final WorkspaceStatus wsStatus = getWorkspaceStatus();
            boolean doExit = false;

            if ( getTransaction().hasChanges() ) {
                if ( force ) {
                    wsStatus.rollback( ExitCommand.class.getName() );
                    doExit = true;
                } else if ( save ) {
                    wsStatus.commit( ExitCommand.class.getName() );
                    doExit = true;
                }
            } else {
                doExit = true;
            }

            if ( doExit ) {
                wsStatus.getShell().exit();
                return CommandResult.SUCCESS;
            }

            // transaction has unsaved changes and neither save or force quit arg was present
            return new CommandResultImpl( false, I18n.bind( ShellI18n.exitCanceled ), null );
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

    /**
     * {@inheritDoc}
     * <p>
     * Value is {@value}.
     *
     * @see org.komodo.shell.BuiltInShellCommand#isOverridable()
     */
    @Override
    public boolean isOverridable() {
        return false;
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
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ShellI18n.exitHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ShellI18n.exitExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ShellI18n.exitUsage ) );
    }

}
