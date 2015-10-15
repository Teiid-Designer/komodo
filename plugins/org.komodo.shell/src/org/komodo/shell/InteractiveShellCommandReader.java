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
package org.komodo.shell;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import org.jboss.aesh.console.Console;
import org.jboss.aesh.console.ConsoleOutput;
import org.jboss.aesh.console.Prompt;
import org.jboss.aesh.console.settings.Settings;
import org.komodo.shell.api.ShellCommandFactory;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * An implementation of the {@link ShellCommandReader} that uses JLine to provide
 * a rich console experience to the user, complete with history, tab completion,
 * and ansi output.
 *
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered to use WorkspaceStatus
 *
 * @author eric.wittmann@redhat.com
 */
public class InteractiveShellCommandReader extends AbstractShellCommandReader {

	private static final String ANSI_BOLD_RED = "\033[1m\033[31m"; //$NON-NLS-1$
	//private static final String ANSI_BOLD_GREEN = "\033[1m\033[32m"; //$NON-NLS-1$
	private static final String ANSI_RESET = "\033[0m "; //$NON-NLS-1$

    private Console consoleReader;

	/**
	 * Constructor.
	 * @param factory shell command factory
	 * @param wsStatus the workspace status
	 */
	public InteractiveShellCommandReader(ShellCommandFactory factory, WorkspaceStatus wsStatus) {
		super(factory, wsStatus);
	}

	/**
	 * @see org.komodo.shell.AbstractShellCommandReader#open()
	 */
	@Override
	public void open() throws Exception {
        Settings settings = Settings.getInstance();
        settings.setAliasEnabled(false);
        // settings.setAliasFile(new File("al"));
        settings.setEnablePipelineAndRedirectionParser(false);
        settings.setLogging(true);

        consoleReader = new Console(settings);
        consoleReader.addCompletion(new TabCompleter(getFactory()));
	}

	/**
	 * Creates the ANSI compatible prompt.
	 * @throws Exception
	 */
	private String defaultAnsiPrompt() throws Exception {
        return ANSI_BOLD_RED + getPrompt() + " > " + ANSI_RESET; //$NON-NLS-1$
	}

	/**
	 * Creates the ANSI compatible prompt.
	 */
//	private String connectedAnsiPrompt(String serverName) throws Exception {
//		return ANSI_BOLD_GREEN + getPrompt() + " " + serverName + " > " + ANSI_RESET; //$NON-NLS-1$ //$NON-NLS-2$
//	}

	private Prompt doGetPrompt() throws Exception {
//	    if(getWorkspaceStatus().hasConnectedTeiid()) {
//	        String serverName = getWorkspaceStatus().getTeiid().getName(getWorkspaceStatus().getTransaction());
//	        return new Prompt(connectedAnsiPrompt(serverName));
//	    }
	    return new Prompt(defaultAnsiPrompt());
	}

	/**
	 * @see org.komodo.common.shell.AbstractShellCommandReader#readLine()
	 */
	@Override
	protected String readLine() throws IOException {
        try {
            final ConsoleOutput output = consoleReader.read( doGetPrompt(), null );
            return output.getBuffer();
        } catch ( Exception ex ) {
            throw new IOException( ex );
        }
	}

	/**
	 * @see org.komodo.common.shell.AbstractShellCommandReader#getOutputWriter()
	 */
	@Override
	protected Writer getOutputWriter() {
        return new OutputStreamWriter(Settings.getInstance().getStdOut());
	}

	/**
	 * @see org.komodo.shell.ShellCommandReader#close()
	 */
	@Override
	public void close() throws IOException {
        consoleReader.stop();
	}

	/**
	 * @see org.komodo.shell.ShellCommandReader#promptForInput(java.lang.String)
	 */
	@Override
    public String promptForInput(String promptString) {
	    try {
            return this.consoleReader.read(doGetPrompt().getPromptAsString()).getBuffer();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
	}

	/**
	 * @see org.komodo.shell.ShellCommandReader#promptForPassword(java.lang.String)
	 */
	@Override
	public String promptForPassword(String promptString) {
        try {
            Prompt newPrompt = doGetPrompt();
            return this.consoleReader.read(newPrompt,Character.valueOf((char) 0)).getBuffer();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
	}

}
