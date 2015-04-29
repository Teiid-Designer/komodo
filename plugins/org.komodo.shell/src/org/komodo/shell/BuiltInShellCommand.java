/*
 * Copyright 2013 JBoss Inc
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
package org.komodo.shell;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.komodo.shell.api.AbstractShellCommand;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.utils.StringUtils;

/**
 * Abstract base class for all built-in shell commands.
 * 
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered to use different Messages class
 * 
 */
public abstract class BuiltInShellCommand extends AbstractShellCommand {

    /**
     * Constructor
	 * @param commandName the command name
	 * @param wsStatus workspace status
	 */
	public BuiltInShellCommand(String commandName, WorkspaceStatus wsStatus) {
		super();
		setName(commandName);
		setWorkspaceStatus(wsStatus);
		initValidWsContextTypes();
	}

	/**
     * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
     */
    @Override
    public void printUsage(int indent) {
        print(indent,Messages.getString(getClass().getSimpleName() + ".usage")); //$NON-NLS-1$
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
     */
    @Override
    public void printHelp(int indent) {
        print(indent,Messages.getString(getClass().getSimpleName() + ".help")); //$NON-NLS-1$
    }
    
    /**
     * @see org.komodo.shell.api.ShellCommand#recordCommand(Arguments)
     */
    @Override
    public void recordCommand(Arguments args) {
    	StringBuffer buff = new StringBuffer(getName());
    	for(int i=0; i<args.size(); i++) {
    		buff.append(" "+args.get(i)); //$NON-NLS-1$
    	}
    	recordToFile(buff.toString());
    }
    
    /**
     * @see org.komodo.shell.api.ShellCommand#recordComment(String)
     */
    @Override
    public void recordComment(String comment) {   
    	recordToFile("# "+comment);  //$NON-NLS-1$
    }
    
    /**
     * Write the supplied line to the recording output file.
     * @param line the line to output
     */
    private void recordToFile(String line) {
    	File outputFile = getWorkspaceStatus().getRecordingOutputFile();
    	if(outputFile!=null) {
    		FileWriter recordingFileWriter = null;
    		try {
    			// Create file if it doesnt exist
            	outputFile.createNewFile();
				recordingFileWriter = new FileWriter(outputFile,true);
				recordingFileWriter.write(line+"\n"); //$NON-NLS-1$ 
				recordingFileWriter.flush();
			} catch (IOException ex) {
	            print(0,"*** Could not create or write to the specifed recording file - "+outputFile); //$NON-NLS-1$
			}
    	    finally {
    	        try {
    	        	recordingFileWriter.close();
    	        } catch (final IOException ignored) {
    	            // ignore
    	        }
    	    }
        // Print error message if the recording file was not defined
    	} else {
            print(0,"*** Recording file not defined in startup properties"); //$NON-NLS-1$
    	}
    }
    
	/**
	 * Validates whether the supplied path is valid.  If the path is relative this takes into account the 
	 * current context.  If invalid an error message is printed out.
	 * @param pathArg the path to test
	 * @return 'true' if the path is valid, 'false' if not.
	 */
	public boolean validatePath(String pathArg) {
		String path = pathArg.trim();
		if(path.length()==0) {
            print(CompletionConstants.MESSAGE_INDENT,Messages.getString("BuiltInShellCommand.locationArg_empty")); //$NON-NLS-1$
			return false;
		}

		WorkspaceStatus wsStatus = getWorkspaceStatus();
		WorkspaceContext newContext = ContextUtils.getContextForPath(wsStatus, pathArg);
				
		if(newContext==null) {
            print(CompletionConstants.MESSAGE_INDENT,Messages.getString("BuiltInShellCommand.locationArg_noContextWithThisName", path)); //$NON-NLS-1$
			return false;
		}
		return true;
	}
	
	/**
	 * Validates whether the supplied object type is a valid child for the supplied context. 
	 * If invalid an error message is printed out.
	 * @param objType the object type
	 * @param context the workspace context
	 * @return 'true' if the child type is valid for the context, 'false' if not.
	 */
	public boolean validateChildType(String objType, WorkspaceContext context) {
		// TODO: add logic to test
		return true;
	}
	
	/**
	 * Validates whether the supplied property is a valid property for the supplied context. 
	 * If invalid an error message is printed out.
	 * @param propName the property name
	 * @param context the workspace context
	 * @return 'true' if the property is valid for the context, 'false' if not.
	 * @exception Exception the exception
	 */
	public boolean validateProperty(String propName, WorkspaceContext context) throws Exception {
		if(!StringUtils.isEmpty(propName)) {
			// Get properties for this object
			List<String> propNames = context.getProperties();
			return propNames.contains(propName) ? true : false;
		}
		return false;
	}
	
	/**
	 * Validates whether the supplied property value is valid for the property 
	 * If invalid an error message is printed out.
	 * @param propName the property name
	 * @param propValue the property value
	 * @param context the workspace context
	 * @return 'true' if the property is valid for the context, 'false' if not.
	 */
	public boolean validatePropertyValue(String propName, String propValue, WorkspaceContext context) {
		// TODO: add logic to test
		return true;
	}
	
	/**
	 * Validate whether the supplied propName is valid for the supplied context.  If invalid, a message is printed out.
	 * @param context the context
	 * @param propName the property name
	 * @return 'true' if valid, 'false' if not.
	 * @throws Exception exception if problem getting the value.
	 */
	public boolean validatePropertyName(WorkspaceContext context, String propName) throws Exception {
		// Check for valid propName
		String propValue = context.getPropertyValue(propName);

		// If a property with the supplied name is not found, propValue is null.
		if(propValue==null) {
            print(CompletionConstants.MESSAGE_INDENT,Messages.getString("BuiltInShellCommand.propertyArg_noPropertyWithThisName", propName)); //$NON-NLS-1$
			return false;
		}
		return true;
	}
	
	/**
	 * Validates whether the supplied property value is valid for the specified global property 
	 * If invalid an error message is printed out.
	 * @param propName the property name
	 * @param propValue the property value
	 * @return 'true' if the property is valid for the context, 'false' if not.
	 */
	public boolean validateGlobalPropertyValue(String propName, String propValue) {
		// TODO: add logic to test
		return true;
	}
	
	/**
	 * Validate whether the supplied propName is a valid global property.  If invalid, a message is printed out.
	 * @param propName the property name
	 * @return 'true' if valid, 'false' if not.
	 * @throws Exception exception if problem getting the value.
	 */
	public boolean validateGlobalProperty(String propName) throws Exception {
		if(WorkspaceStatus.GLOBAL_PROP_KEYS.contains(propName.toUpperCase())) {
			return true;
		}
		return false;
	}
	
    /**
     * Updates the candidates for tab completion, given the currentContext and path
     * @param candidates the candidates list
     * @param currentContext the current context
     * @param includeGoUp if 'true' the '..' option is included
     * @param lastArgument the last arg
     * @throws Exception the exception
     */
    public void updateTabCompleteCandidatesForPath(List<CharSequence> candidates, WorkspaceContext currentContext, boolean includeGoUp, String lastArgument) throws Exception {
    	// List of potentials completions
    	List<String> potentialsList = new ArrayList<String>();
    	// Only offer '..' if below the root
    	if( (currentContext.getType()!=WorkspaceStatus.ROOT_TYPE) && includeGoUp ) {
    		potentialsList.add(StringConstants.DOT_DOT);
    	}

    	// --------------------------------------------------------------
    	// No arg - offer children relative current context.
    	// --------------------------------------------------------------
    	if(lastArgument==null) {
    		List<WorkspaceContext> children = currentContext.getChildren();
    		for(WorkspaceContext wsContext : children) {
    			potentialsList.add(wsContext.getName()+ContextUtils.PATH_SEPARATOR);
    		}
    		candidates.addAll(potentialsList);
    		// --------------------------------------------------------------
    		// One arg - determine the completion options for it.
    		// --------------------------------------------------------------
    	} else {
    		// --------------------------------------------
    		// Absolute Path Arg handling
    		// --------------------------------------------
    		if( lastArgument.startsWith(ContextUtils.PATH_SEPARATOR) ) {
    			// If not the full absolute root, then provide it
    			if(!ContextUtils.isAbsolutePath(lastArgument)) {
    				potentialsList.add(ContextUtils.PATH_SEPARATOR+ContextUtils.ROOT_CONTEXT_NAME+ContextUtils.PATH_SEPARATOR);
    				updateCandidates(candidates,potentialsList,lastArgument);
    				// Starts with correct root - provide next option
    			} else {
    				String relativePath = ContextUtils.convertAbsolutePathToRootRelative(lastArgument);
    				WorkspaceContext deepestMatchingContext = ContextUtils.getDeepestMatchingContextRelative(getWorkspaceStatus().getRootContext(), relativePath);

    				// Get children of deepest context match to form potentialsList
    				List<WorkspaceContext> children = deepestMatchingContext.getChildren();
    				if(!children.isEmpty()) {
    					// Get all children as potentials
    					for(WorkspaceContext childContext : children) {
    						String absolutePath = ContextUtils.PATH_SEPARATOR + childContext.getFullName();
    						potentialsList.add(absolutePath+ContextUtils.PATH_SEPARATOR);
    					}
    				} else {
    					String absolutePath = ContextUtils.PATH_SEPARATOR + deepestMatchingContext.getFullName();
    					potentialsList.add(absolutePath+ContextUtils.PATH_SEPARATOR);
    				}
    				updateCandidates(candidates, potentialsList, lastArgument);
    			}
    			// -------------------------------------------
    			// Relative Path Arg handling
    			// -------------------------------------------
    		} else {
    			// Deepest matching context for relative path
    			WorkspaceContext deepestMatchingContext = ContextUtils.getDeepestMatchingContextRelative(currentContext, lastArgument);

    			// Get children of deepest context match to form potentialsList
    			List<WorkspaceContext> children = deepestMatchingContext.getChildren();
    			if(!children.isEmpty()) {
    				// Get all children as potentials
    				for(WorkspaceContext childContext : children) {
    					String absolutePath = ContextUtils.PATH_SEPARATOR + childContext.getFullName();
    					String relativePath = ContextUtils.convertAbsolutePathToRelative(currentContext, absolutePath);
    					potentialsList.add(relativePath+ContextUtils.PATH_SEPARATOR);
    				}
    			} else {
    				String absolutePath = ContextUtils.PATH_SEPARATOR + deepestMatchingContext.getFullName();
    				String relativePath = ContextUtils.convertAbsolutePathToRelative(currentContext, absolutePath);
    				potentialsList.add(relativePath+ContextUtils.PATH_SEPARATOR);
    			}
    			updateCandidates(candidates, potentialsList, lastArgument);
    		}
    
    	}
    }
    
    /**
     * Updates the candidates for tab completion, given the context and property Arg
     * @param candidates the candidates list
     * @param context the context
     * @param propArg the propName for completion
     * @throws Exception the exception
     */
    public void updateTabCompleteCandidatesForProperty(List<CharSequence> candidates, WorkspaceContext context, String propArg) throws Exception {
		// List of potentials completions
		List<String> potentialsList = new ArrayList<String>();

		// Context properties
		List<String> currentProps = context.getProperties();  // All properties

		// Sort the properties by name
		List<String> sortedPropNames = new ArrayList<String>(currentProps);
		Collections.sort(sortedPropNames);

		potentialsList.addAll(sortedPropNames);
		if(StringUtils.isEmpty(propArg)) {
			candidates.addAll(potentialsList);
		} else {
			updateCandidates(candidates, potentialsList, propArg);
		}
    }

    /**
     * Adds the valid items from the completionList to the candidates.  They are added to the candidates if they start
     * with 'lastArg'
     * @param candidates the candidates
     * @param completionList possibilities before filtering based on last arg
     * @param lastArg the commandline arg
     */
    private void updateCandidates(List<CharSequence> candidates, List<String> completionList, String lastArg) {
    	for (String item : completionList) {
    		if (item.toUpperCase().startsWith(lastArg.toUpperCase())) {
    			candidates.add(item);
    		}
    	}
    }

}
