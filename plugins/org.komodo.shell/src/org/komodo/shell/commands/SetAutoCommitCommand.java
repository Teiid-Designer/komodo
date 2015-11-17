/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands;

import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.i18n.I18n;

/**
 * A {@link ShellCommand command} that can enable or disable the automatic committing of commands.
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * set-auto-commit &lt;true | false&gt;
 * </code>
 */
public class SetAutoCommitCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "set-auto-commit"; //$NON-NLS-1$

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public SetAutoCommitCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            final String arg = requiredArgument( 0, I18n.bind( ShellI18n.enable_flag_missing ) );

            // Check for invalid arg
            if ( !arg.equalsIgnoreCase( Boolean.TRUE.toString() ) && !arg.equalsIgnoreCase( Boolean.FALSE.toString() ) ) {
                return new CommandResultImpl( false,
                                              I18n.bind( ShellI18n.invalidBooleanGlobalPropertyValue,
                                                         arg,
                                                         WorkspaceStatus.AUTO_COMMIT ),
                                              null );
            }

            getWorkspaceStatus().setProperty(WorkspaceStatus.AUTO_COMMIT, arg);

            return new CommandResultImpl( I18n.bind( ShellI18n.globalPropertySet, WorkspaceStatus.AUTO_COMMIT ) );
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
        print( indent, I18n.bind( ShellI18n.setAutoCommitHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ShellI18n.setAutoCommitExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ShellI18n.setAutoCommitUsage ) );
    }

    /**
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        if ( getArguments().size() == 0 ) {
            updateCandidatesForBooleanProperty( lastArgument, candidates );
        }

        return ( candidates.isEmpty() ? -1 : ( toString().length() + 1 ) );
    }

}
