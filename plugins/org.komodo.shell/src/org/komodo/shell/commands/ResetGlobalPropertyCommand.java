package org.komodo.shell.commands;

import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

public class ResetGlobalPropertyCommand extends BuiltInShellCommand {
	/**
	 * The command name.
	 */
	public static final String NAME = "reset-global"; //$NON-NLS-1$

	private final static String ARG_ALL = "--all";

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
				for (final Entry<String, String> entry : WorkspaceStatus.GLOBAL_PROPS.entrySet()) {
					status.setProperty(entry.getKey(), entry.getValue());
				}
				return new CommandResultImpl(I18n.bind(ShellI18n.globalResetAllProps)); //Reset all props OK
			} else { 
				// reset single property
				String validationStatus = status.validateGlobalPropertyValue(firstArgument.toUpperCase(), null);
				if (StringUtils.isEmpty(validationStatus)) {
					status.setProperty(firstArgument.toUpperCase(), null);
					return new CommandResultImpl(I18n.bind(ShellI18n.globalPropertyReset,firstArgument));// Reset one prop OK
				} else { 
					// invalid property name
	                return new CommandResultImpl( false, I18n.bind( ShellI18n.invalidGlobalProperty, validationStatus ), null );
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
	

    public int tabCompletion( final String lastArgument, final List< CharSequence > candidates ) throws Exception {
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
        
        return ( candidates.isEmpty() ? -1 : ( StringUtils.isBlank( lastArgument ) ? 0 : ( toString().length() + 1 ) ) );
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
