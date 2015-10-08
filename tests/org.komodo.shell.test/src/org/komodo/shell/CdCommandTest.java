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
import org.junit.Test;
import org.komodo.shell.commands.CdCommand;
import org.komodo.shell.util.KomodoObjectUtils;

/**
 * Test Class to test CdCommand
 *
 */
@SuppressWarnings("javadoc")
public class CdCommandTest extends AbstractCommandTest {

	private static final String CD_COMMAND_REL1 = "cdCommand_Relative1.txt"; //$NON-NLS-1$
	private static final String CD_COMMAND_ABS1 = "cdCommand_Absolute1.txt"; //$NON-NLS-1$
    private static final String ALIAS_TEST = "cdCommand_aliases.txt"; //$NON-NLS-1$

    /**
	 * Test for CdCommand
	 */
	public CdCommandTest( ) {
		super();
	}

    @Test
    public void testCdRelative1() throws Exception {
    	setup(CD_COMMAND_REL1, CdCommand.class);

    	execute();

    	// Check WorkspaceContext
    	assertEquals("/workspace", KomodoObjectUtils.getFullName(wsStatus, wsStatus.getCurrentContext())); //$NON-NLS-1$
    }

    @Test
    public void testCdAbsolute1() throws Exception {
    	setup(CD_COMMAND_ABS1, CdCommand.class);

    	execute();

    	// Check WorkspaceContext
    	String contextName =  KomodoObjectUtils.getFullName(wsStatus, wsStatus.getCurrentContext());
    	assertEquals("/workspace", contextName); //$NON-NLS-1$
    }

    @Test
    public void testAliases() throws Exception {
        setup(ALIAS_TEST, CdCommand.class);

        execute();

        // Check WorkspaceContext
        assertEquals("/workspace/MyVdb",  KomodoObjectUtils.getFullName(wsStatus, wsStatus.getCurrentContext())); //$NON-NLS-1$
    }

}
