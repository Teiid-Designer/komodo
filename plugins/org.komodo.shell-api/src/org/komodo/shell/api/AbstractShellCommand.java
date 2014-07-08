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
package org.komodo.shell.api;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.List;

/**
 * Base class for shell commands.
 * 
 * This class adapted from classes at https://github.com/Governance/s-ramp/blob/master/s-ramp-shell-api
 * - altered to use WorkspaceStatus
 * 
 * @author eric.wittmann@redhat.com
 */
public abstract class AbstractShellCommand implements ShellCommand {

	private WorkspaceStatus wsStatus;
	private Arguments arguments;
	private Writer writer;
	private List<WorkspaceContext.Type> validWsContextTypes;
	private String name;

	/**
	 * Constructor.
	 */
	public AbstractShellCommand() {
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#getName()
	 */
	@Override
	public String getName() {
		return this.name;
	}
	
	/**
	 * @see org.komodo.shell.api.ShellCommand#setName(String)
	 */
	@Override
	public void setName(String name) {
		this.name=name;
	}
	
	/**
	 * @see org.komodo.shell.api.ShellCommand#initValidWsContextTypes()
	 */
	@Override
	public void initValidWsContextTypes() {
		// default is valid for all workspace context types
		List<WorkspaceContext.Type> validTypes = new ArrayList<WorkspaceContext.Type>(1);
		validTypes.add(WorkspaceContext.Type.ALL);
		this.validWsContextTypes = validTypes;
	}

	/**
	 * @return the list of valid Ws context
	 */
	public List<WorkspaceContext.Type> getValidWsContextTypes( ) {
		return this.validWsContextTypes;
	}
	
	/**
	 * @see org.komodo.shell.api.ShellCommand#isValidForWsContext(WorkspaceContext.Type)
	 */
	@Override
	public boolean isValidForWsContext(WorkspaceContext.Type contextType) {
		if(validWsContextTypes.contains(WorkspaceContext.Type.ALL)) {
			return true;
		}
		for(WorkspaceContext.Type cType : validWsContextTypes) {
			if(cType==contextType) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * @see org.komodo.shell.api.ShellCommand#setWorkspaceStatus(org.komodo.shell.api.WorkspaceStatus)
	 */
	@Override
	public void setWorkspaceStatus(WorkspaceStatus wsStatus) {
		this.wsStatus = wsStatus;
	}

	/**
	 * @return the workspace status
	 */
	protected WorkspaceStatus getWorkspaceStatus() {
		return this.wsStatus;
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#setArguments(Arguments)
	 */
	@Override
	public void setArguments(Arguments arguments) {
		this.arguments = arguments;
	}

	/**
	 * Gets the command's arguments.
	 */
	protected Arguments getArguments() {
		return this.arguments;
	}

	/**
	 * Returns the argument at the given index.  Throws an exception if the argument
	 * does not exist.
	 * @param argIndex
	 * @param message
	 * @throws InvalidCommandArgumentException
	 */
	protected String requiredArgument(int argIndex, String message) throws InvalidCommandArgumentException {
		if (getArguments().size() <= argIndex) {
			throw new InvalidCommandArgumentException(argIndex, message);
		}
		return getArguments().get(argIndex);
	}

	/**
	 * Returns the optional argument at the given index.  Returns null if the argument
	 * does not exist.
	 * @param argIndex
	 */
	protected String optionalArgument(int argIndex) {
		return optionalArgument(argIndex, null);
	}

	/**
	 * Returns the optional argument at the given index.  Returns the given default value if
	 * the argument does not exist.
	 * @param argIndex
	 * @param defaultValue
	 */
	protected String optionalArgument(int argIndex, String defaultValue) {
		if (getArguments().size() <= argIndex) {
			return defaultValue;
		}
		return getArguments().get(argIndex);
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#print(java.lang.String, java.lang.Object[])
	 */
	@Override
	public void print(String formattedMessage, Object... params) {
		String msg = String.format(formattedMessage, params);
		if (writer != null) {
			try {
				writer.write(msg);
				writer.write('\n');
				writer.flush();
			} catch (IOException e) {
			}
		} else {
			System.out.println(msg);
		}
	}

	/**
	 * @param output the output to set
	 */
	@Override
	public void setOutput(Writer output) {
		this.writer = output;
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#tabCompletion(java.lang.String, java.util.List)
	 */
	@Override
	public int tabCompletion(String lastArgument, List<CharSequence> candidates) {
		return -1;
	}

}
