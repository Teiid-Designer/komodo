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
import org.komodo.shell.commands.core.ShowCommand;

/**
 * Test Class to test ShowCommand
 *
 */
@SuppressWarnings({"javadoc", "nls"})
public class ShowCommandTest extends AbstractCommandTest {

	private static final String SHOW_STATUS1 = "showStatus1.txt"; //$NON-NLS-1$
	private static final String SHOW_STATUS2 = "showStatus2.txt"; //$NON-NLS-1$
	private static final String SHOW_CHILDREN1 = "showChildren1.txt"; //$NON-NLS-1$
	private static final String SHOW_CHILDREN2 = "showChildren2.txt"; //$NON-NLS-1$
	private static final String INDENT = getIndentStr();

	/**
	 * Test for StatusCommand
	 */
	public ShowCommandTest( ) {
		super();
	}

    @Test
    public void testShowStatus1() throws Exception {
    	setup(SHOW_STATUS1, ShowCommand.class);

    	execute();

    	// make sure repository URL and workspace appear, no Teiid is set, and current context path
    	String writerOutput = getCommandOutput();
        assertTrue(writerOutput.contains("test-local-repository-in-memory-config.json"));
        assertTrue(writerOutput.contains("Workspace : komodoLocalWorkspace"));
        assertTrue(writerOutput.contains("None set"));
        assertTrue(writerOutput.contains("/workspace"));

    	assertEquals("/workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testShowStatus2() throws Exception {
    	setup(SHOW_STATUS2, ShowCommand.class);

    	execute();

        // make sure repository URL and workspace appear, no Teiid is set, and current context path
        String writerOutput = getCommandOutput();
        assertTrue(writerOutput.contains("test-local-repository-in-memory-config.json"));
        assertTrue(writerOutput.contains("Workspace : komodoLocalWorkspace"));
        assertTrue(writerOutput.contains("None set"));
        assertTrue(writerOutput.contains("/workspace/MyVdb/MyModel"));

        assertEquals("/workspace/MyVdb/MyModel", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testShowChildren1() throws Exception {
    	setup(SHOW_CHILDREN1, ShowCommand.class);

    	execute();

    	String expectedOutput = INDENT+"There are no children for Workspace \"/workspace\".\n"; //$NON-NLS-1$

    	String writerOutput = getCommandOutput();
    	assertEquals(expectedOutput,writerOutput);
    	assertEquals("/workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testShowChildren2() throws Exception {
    	setup(SHOW_CHILDREN2, ShowCommand.class);

    	execute();

        // make sure model names and model type appear in output
        String writerOutput = getCommandOutput();
        assertTrue( writerOutput.contains( "Model1" ) );
        assertTrue( writerOutput.contains( "Model2" ) );
        assertTrue( writerOutput.contains( "Model3" ) );

        assertEquals("/workspace/MyVdb", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

}
