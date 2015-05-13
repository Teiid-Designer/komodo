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
import org.junit.Ignore;
import org.junit.Test;
import org.komodo.shell.commands.core.ShowCommand;

/**
 * Test Class to test ShowCommand
 *
 */
@SuppressWarnings("javadoc")
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

	@Ignore
    @Test
    public void testShowStatus1() throws Exception {
    	setup(SHOW_STATUS1, ShowCommand.class);

    	execute();

    	String expectedOutput = INDENT+"Current Repo    : local Repository\n"+ //$NON-NLS-1$
                                        INDENT+"Current Teiid Instance  : None set\n"+ //$NON-NLS-1$
                                        INDENT+"Current Context : [/tko:workspace]\n"; //$NON-NLS-1$

    	String writerOutput = getCommandOutput();
    	assertEquals(expectedOutput,writerOutput);
    	assertEquals("/tko:workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testShowStatus2() throws Exception {
    	setup(SHOW_STATUS2, ShowCommand.class);

    	execute();

    	String expectedOutput = INDENT+"Current Repo    : local Repository\n"+ //$NON-NLS-1$
                                INDENT+"Current Teiid Instance  : None set\n"+ //$NON-NLS-1$
    	                        INDENT+"Current Context : [/tko:workspace/MyVdb/MyModel]\n"; //$NON-NLS-1$
    	String writerOutput = getCommandOutput();
    	assertEquals(expectedOutput,writerOutput);
    	assertEquals("/tko:workspace/MyVdb/MyModel", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Ignore
    @Test
    public void testShowChildren1() throws Exception {
    	setup(SHOW_CHILDREN1, ShowCommand.class);

    	execute();

    	String expectedOutput = INDENT+"No children for WORKSPACE \"/tko:workspace\".\n"; //$NON-NLS-1$

    	String writerOutput = getCommandOutput();
    	assertEquals(expectedOutput,writerOutput);
    	assertEquals("/tko:workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testShowChildren2() throws Exception {
    	setup(SHOW_CHILDREN2, ShowCommand.class);

    	execute();

    	String expectedOutput = INDENT+"Children for VDB \"/tko:workspace/MyVdb\"\n"+ //$NON-NLS-1$
                                INDENT+"---------------          \n"+ //$NON-NLS-1$
    	                        INDENT+"Model1 [MODEL]\n"+ //$NON-NLS-1$
    	                        INDENT+"Model2 [MODEL]\n"+ //$NON-NLS-1$
    	                        INDENT+"Model3 [MODEL]\n"; //$NON-NLS-1$

    	String writerOutput = getCommandOutput();
    	assertEquals(expectedOutput,writerOutput);
    	assertEquals("/tko:workspace/MyVdb", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

}
