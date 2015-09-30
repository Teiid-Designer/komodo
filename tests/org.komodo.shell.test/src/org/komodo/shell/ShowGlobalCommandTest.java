/*
 * Copyright 2014 JBoss Inc
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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.komodo.shell.commands.core.ShowStatusCommand;

/**
 * Test Class to test ShowStatusCommand
 *
 */
@SuppressWarnings({"javadoc", "nls"})
public class ShowGlobalCommandTest extends AbstractCommandTest {

    private static final String SHOW_STATUS1 = "showStatus1.txt";

	/**
	 * Test for StatusCommand
	 */
	public ShowGlobalCommandTest( ) {
		super();
	}

    @Test
    public void testShowStatus1() throws Exception {
    	setup(SHOW_STATUS1, ShowStatusCommand.class);

    	execute();

    	// make sure repository URL and workspace appear, no Teiid is set, and current context path
    	String writerOutput = getCommandOutput();
        assertTrue(writerOutput.contains("test-local-repository-in-memory-config.json"));
        assertTrue(writerOutput.contains("Name : komodoLocalWorkspace"));
        assertTrue(writerOutput.contains("None set"));
        assertTrue(writerOutput.contains("/workspace"));

    	assertEquals("/workspace", wsStatus.getCurrentContextFullName()); //$NON-NLS-1$
    }

}
