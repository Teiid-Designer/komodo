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
import org.komodo.shell.commands.core.ListCommand;

/**
 * Test Class to test ListCommand
 *
 */
@SuppressWarnings({"javadoc", "nls"})
public class ListCommandTest extends AbstractCommandTest {

	private static final String LIST_COMMAND1 = "listCommand1.txt"; //$NON-NLS-1$
	private static final String LIST_COMMAND2 = "listCommand2.txt"; //$NON-NLS-1$
	private static final String LIST_COMMAND3 = "listCommand3.txt"; //$NON-NLS-1$

	/**
	 * Test for ListCommand
	 */
	public ListCommandTest( ) {
		super();
	}

    @Test
    public void testList1() throws Exception {
    	setup(LIST_COMMAND1, ListCommand.class);

    	execute();

        // make sure no children and path appear in output
    	String writerOutput = getCommandOutput();
        assertTrue( writerOutput.contains( "no children" ) );
        assertTrue( writerOutput.contains( "Workspace \"/workspace\"" ) );

        assertEquals("/workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testList2() throws Exception {
    	setup(LIST_COMMAND2, ListCommand.class);

    	execute();

        // make sure model names and model type appear in output
        String writerOutput = getCommandOutput();
        assertTrue( writerOutput.contains( "Model1" ) );
        assertTrue( writerOutput.contains( "Model2" ) );
        assertTrue( writerOutput.contains( "Model3" ) );

    	assertEquals("/workspace/MyVdb", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testList3() throws Exception {
    	setup(LIST_COMMAND3, ListCommand.class);

    	execute();

        // make sure table name and table type appear in output
        String writerOutput = getCommandOutput();
        assertTrue( writerOutput.contains( "Table1" ) );

        assertEquals("/workspace/MyVdb/Model1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

}
