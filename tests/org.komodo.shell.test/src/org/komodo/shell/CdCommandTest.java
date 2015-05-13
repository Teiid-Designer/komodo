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
import org.komodo.shell.commands.core.CdCommand;

/**
 * Test Class to test CdCommand
 *
 */
@SuppressWarnings("javadoc")
public class CdCommandTest extends AbstractCommandTest {

	private static final String CD_COMMAND_REL1 = "cdCommand_Relative1.txt"; //$NON-NLS-1$
	private static final String CD_COMMAND_REL2 = "cdCommand_Relative2.txt"; //$NON-NLS-1$
	private static final String CD_COMMAND_REL3 = "cdCommand_Relative3.txt"; //$NON-NLS-1$
	private static final String CD_COMMAND_REL4 = "cdCommand_Relative4.txt"; //$NON-NLS-1$
	private static final String CD_COMMAND_REL5 = "cdCommand_Relative5.txt"; //$NON-NLS-1$
	private static final String CD_COMMAND_REL6 = "cdCommand_Relative6.txt"; //$NON-NLS-1$

	private static final String CD_COMMAND_ABS1 = "cdCommand_Absolute1.txt"; //$NON-NLS-1$
	private static final String CD_COMMAND_ABS2 = "cdCommand_Absolute2.txt"; //$NON-NLS-1$
	private static final String CD_COMMAND_ABS3 = "cdCommand_Absolute3.txt"; //$NON-NLS-1$
	private static final String CD_COMMAND_ABS4 = "cdCommand_Absolute4.txt"; //$NON-NLS-1$

	/**
	 * Test for CdCommand
	 */
	public CdCommandTest( ) {
		super();
	}

	@Ignore
    @Test
    public void testCdRelative1() throws Exception {
    	setup(CD_COMMAND_REL1, CdCommand.class);

    	execute();

    	// Check WorkspaceContext
    	assertEquals("/tko:workspace", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testCdRelative2() throws Exception {
    	setup(CD_COMMAND_REL2, CdCommand.class);

    	execute();

    	// Check WorkspaceContext
    	assertEquals("/tko:workspace/MyVdb/MyModel", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testCdRelative3() throws Exception {
    	setup(CD_COMMAND_REL3, CdCommand.class);

    	execute();

    	// Check WorkspaceContext
    	assertEquals("/tko:workspace/MyVdb/MyModel", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testCdRelative4() throws Exception {
    	setup(CD_COMMAND_REL4, CdCommand.class);

    	execute();

    	// Check WorkspaceContext
    	assertEquals("/tko:workspace/MyVdb", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testCdRelative5() throws Exception {
    	setup(CD_COMMAND_REL5, CdCommand.class);

    	execute();

    	// Check WorkspaceContext
    	assertEquals("/tko:workspace/MyVdb/MyModel/MyTable1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testCdRelative6() throws Exception {
    	setup(CD_COMMAND_REL6, CdCommand.class);

    	execute();

    	// Check WorkspaceContext
    	assertEquals("/tko:workspace/MyVdb/MyModel/MyTable1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Ignore
    @Test
    public void testCdAbsolute1() throws Exception {
    	setup(CD_COMMAND_ABS1, CdCommand.class);

    	execute();

    	// Check WorkspaceContext
    	String contextName = wsStatus.getCurrentContext().getFullName();
    	assertEquals("/tko:workspace", contextName); //$NON-NLS-1$
    }

    @Test
    public void testCdAbsolute2() throws Exception {
    	setup(CD_COMMAND_ABS2, CdCommand.class);

    	execute();

    	// Check WorkspaceContext
    	assertEquals("/tko:workspace/MyVdb", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testCdAbsolute3() throws Exception {
    	setup(CD_COMMAND_ABS3, CdCommand.class);

    	execute();

    	// Check WorkspaceContext
    	assertEquals("/tko:workspace/MyVdb/MyModel/MyTable2", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

    @Test
    public void testCdAbsolute4() throws Exception {
    	setup(CD_COMMAND_ABS4, CdCommand.class);

    	execute();

    	// Check WorkspaceContext
    	assertEquals("/tko:workspace/MyVdb/MyModel/Table1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }

}
