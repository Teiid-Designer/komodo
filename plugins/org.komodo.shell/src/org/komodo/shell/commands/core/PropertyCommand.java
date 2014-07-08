/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.commands.core;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.Messages;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;

/**
 *
 */
public class PropertyCommand extends BuiltInShellCommand {

	/**
	 * Constructor.
	 * @param name the command name
	 * @param wsStatus the workspace status
	 */
	public PropertyCommand(String name, WorkspaceStatus wsStatus) {
		super(name, wsStatus);
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#execute()
	 */
	@Override
	public boolean execute() throws Exception {
		String subcmdArg = requiredArgument(0, Messages.getString("PropertyCommand.InvalidArgMsg_SubCommand")); //$NON-NLS-1$
		
		// Check required args
		String propNameArg = null;
		String propValueArg = null;
		if (!"show".equals(subcmdArg)) { //$NON-NLS-1$
			propNameArg = requiredArgument(1, Messages.getString("PropertyCommand.InvalidArgMsg_PropertyName")); //$NON-NLS-1$
			if ("set".equals(subcmdArg)) { //$NON-NLS-1$
				propValueArg = requiredArgument(2, Messages.getString("PropertyCommand.InvalidArgMsg_PropertyValue")); //$NON-NLS-1$
			}
		}

		try {
			if ("show".equals(subcmdArg)) { //$NON-NLS-1$
				Map<String,String> currentProps = getPropNamesForContext(getWorkspaceStatus());
				print("Current Properties:\n-------------------\n"); //$NON-NLS-1$
				for(String pName : currentProps.keySet()) {
					String propStr = pName + " : " + currentProps.get(pName);  //$NON-NLS-1$
					print(propStr);
				}
			} else if ("set".equals(subcmdArg)) { //$NON-NLS-1$
				setProperty(propNameArg, propValueArg);
				print(Messages.getString("PropertyCommand.PropertySet", propNameArg)); //$NON-NLS-1$
			} else if ("unset".equals(subcmdArg)) { //$NON-NLS-1$
				//unsetProperty(artifact, propNameArg);
				print(Messages.getString("PropertyCommand.PropertyUnset", propNameArg)); //$NON-NLS-1$
			} else {
				throw new InvalidCommandArgumentException(0, Messages.getString("Property.InvalidSubCommand")); //$NON-NLS-1$
			}
		} catch (InvalidCommandArgumentException e) {
			throw e;
		} catch (Exception e) {
			print(Messages.getString("PropertyCommand.Failure")); //$NON-NLS-1$
			print("\t" + e.getMessage()); //$NON-NLS-1$
            return false;
		}
        return true;
	}

	private Map<String,String> getPropNamesForContext(WorkspaceStatus wsStatus) {
		Map<String,String> currentProps = new HashMap<String,String>();
		WorkspaceContext currentContext = wsStatus.getCurrentContext();
		if(currentContext.getType().equals(WorkspaceContext.Type.ROOT)) {
			String recFilePath = wsStatus.getRecordingOutputFile().toString();
			String teiidUrl = wsStatus.getTeiidServerUrl();
			currentProps.put(WorkspaceStatus.RECORDING_FILEPATH_KEY,recFilePath);
			currentProps.put(WorkspaceStatus.TEIID_SERVER_URL_KEY,teiidUrl);
		}
		return currentProps;
	}
	
	/**
	 * Sets a property on the artifact.
	 * @param artifact
	 * @param propName
	 * @param propValue
	 */
	private void setProperty(String propName, String propValue) {
		WorkspaceStatus wsStatus = getWorkspaceStatus();
		WorkspaceContext currentContext = wsStatus.getCurrentContext();
		if(currentContext.getType().equals(WorkspaceContext.Type.ROOT)) {
			if(propName.equals(WorkspaceStatus.RECORDING_FILEPATH_KEY)) {
				wsStatus.setRecordingOutputFile(propValue);
			} else if(propName.equals(WorkspaceStatus.TEIID_SERVER_URL_KEY)) {
				wsStatus.setTeiidServerUrl(propValue);
			}
		}
	}

	/**
	 * Unsets a property on the artifact.
	 * @param artifact
	 * @param propName
	 */
//	private void unsetProperty(BaseArtifactType artifact, String propName) {
//		setProperty(artifact, propName, null);
//	}

	/**
	 * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
	 */
	@Override
	public int tabCompletion(String lastArgument, List<CharSequence> candidates) {

		if (getArguments().isEmpty()) {
			if (lastArgument == null) {
				candidates.add("set "); //$NON-NLS-1$
				candidates.add("show "); //$NON-NLS-1$
				candidates.add("unset "); //$NON-NLS-1$
				return 0;
			} else if ("show".startsWith(lastArgument)) { //$NON-NLS-1$
				candidates.add("show "); //$NON-NLS-1$
				return 0;
			} else if ("set".startsWith(lastArgument)) { //$NON-NLS-1$
				candidates.add("set "); //$NON-NLS-1$
				return 0;
			} else if ("unset".startsWith(lastArgument)) { //$NON-NLS-1$
				candidates.add("unset "); //$NON-NLS-1$
				return 0;
			}
		} else if (getArguments().size() == 1 && 
				(getArguments().contains("set") || getArguments().contains("unset") || getArguments().contains("show"))) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			Map<String,String> currentProps = getPropNamesForContext(getWorkspaceStatus());
			String candidatePostfix = " "; //$NON-NLS-1$
			if (getArguments().contains("unset")) { //$NON-NLS-1$
				candidatePostfix = ""; //$NON-NLS-1$
			}
			for (String prop : currentProps.keySet()) {
				if (lastArgument == null || prop.startsWith(lastArgument)) {
					candidates.add(prop + candidatePostfix);
				}
			}
			return 0;
		}
		return -1;
	}

}