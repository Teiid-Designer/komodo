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

import static org.komodo.shell.Messages.ExitCommand.EXIT_CANCELED;
import static org.komodo.shell.Messages.ExitCommand.GOOD_BYE;
import static org.komodo.shell.Messages.ExitCommand.INVALID_EXIT_ARG;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.Messages;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.StringUtils;

/**
 * Implements the 'exit' command.
 *
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered to use different Message class
 *
 * @author eric.wittmann@redhat.com
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
                final String errorMsg = Messages.getString( INVALID_EXIT_ARG, arg );
                throw new InvalidCommandArgumentException( 0, errorMsg );
            }

            // max of one argument (this is checked by super)
            assert( optionalArgument( 1 ) == null );

            boolean force = ( hasArg && FORCE_ARGS.contains( arg ) );
            boolean save = ( hasArg && SAVE_ARGS.contains( arg ) );

            final WorkspaceStatus wsStatus = getWorkspaceStatus();
            final UnitOfWork uow = wsStatus.getTransaction();
            boolean doExit = false;

            if ( uow.hasChanges() ) {
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
                return new CommandResultImpl( Messages.getString( GOOD_BYE ) );
            }

            // transaction has unsaved changes and neither save or force quit arg was present
            return new CommandResultImpl( false, Messages.getString( EXIT_CANCELED ), null );
        } catch ( final Exception e ) {
            return new CommandResultImpl( false, e.getLocalizedMessage(), e );
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

}
