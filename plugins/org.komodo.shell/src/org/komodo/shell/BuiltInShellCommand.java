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
import org.komodo.shell.api.AbstractShellCommand;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.WorkspaceStatus;

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
			    // ignore
			}
    	    finally {
    	        try {
    	        	recordingFileWriter.close();
    	        } catch (final IOException ignored) {
    	            // ignore
    	        }
    	    }
    	}
    }

}
