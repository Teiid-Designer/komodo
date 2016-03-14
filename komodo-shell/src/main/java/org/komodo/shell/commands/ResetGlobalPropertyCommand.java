package org.komodo.shell.commands;

import java.util.LinkedList;
import java.util.List;

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A {@link BuiltInShellCommand command} that can reset global workspace properties
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * reset-global &lt;propName&gt;
 * </code>
 */
public class ResetGlobalPropertyCommand extends BuiltInShellCommand {
	/**
	 * The command name.
	 */
	public static final String NAME = "reset-global"; //$NON-NLS-1$

	private final static String ARG_ALL = "--all"; //$NON-NLS-1$

    /**
     * @param workspaceStatus
     *        the workspace status (cannot be <code>null</code>)
     */
	public ResetGlobalPropertyCommand(final WorkspaceStatus workspaceStatus) {
		super(workspaceStatus, NAME);
	}

	@Override
	public boolean isValidForCurrentContext() {
		return true;
	}

	@Override
	protected CommandResult doExecute() {
		try {

			String firstArgument = requiredArgument(0, I18n.bind(ShellI18n.invalidArgMsgResetGlobalPropertyName,ARG_ALL));

			WorkspaceStatus status = getWorkspaceStatus();
			if (firstArgument.equals(ARG_ALL)) {
				// reset all global properties
				status.resetGlobalProperties();
				return new CommandResultImpl(I18n.bind(ShellI18n.globalResetAllProps)); //Reset all props OK
			} else {
				// reset single property
				String validationStatus = status.validateGlobalPropertyValue(firstArgument.toUpperCase(), null);
				if (StringUtils.isEmpty(validationStatus)) {
					status.setGlobalProperty(firstArgument.toUpperCase(), null);
					return new CommandResultImpl(I18n.bind(ShellI18n.globalPropertyReset,firstArgument));// Reset one prop OK
				} else {
					// invalid property name or cannot reset
	                return new CommandResultImpl( false, I18n.bind( ShellI18n.invalidGlobalPropertyCannotReset ), null );
				}
			}
		} catch (Exception e) {
			return new CommandResultImpl(e);
		}

	}

	@Override
	protected int getMaxArgCount() {
		return 1;
	}


    @Override
	public TabCompletionModifier tabCompletion( final String lastArgument, final List< CharSequence > candidates ) throws Exception {
        if ( getArguments().size() == 0 ) {
            // Global property completion options and reset all argument
            List< String > potentials = new LinkedList<>(WorkspaceStatus.GLOBAL_PROPS.keySet());
            potentials.add(0,ARG_ALL);

            if ( lastArgument == null ) {
                candidates.addAll( potentials );
            } else {
                for ( final String name : potentials ) {
                    if ( name.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( name );
                    }
                }
            }
        }
        return TabCompletionModifier.AUTO;
    }

	@Override
	protected void printHelpDescription(int indent) {
		print( indent, I18n.bind( ShellI18n.resetGlobalPropertyHelp, getName() ) );
	}

	@Override
	protected void printHelpExamples(int indent) {
        print( indent, I18n.bind( ShellI18n.resetGlobalPropertyExamples ) );

	}

	@Override
	protected void printHelpUsage(int indent) {
        print( indent, I18n.bind( ShellI18n.resetGlobalPropertyUsage ) );

	}


}
