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
import org.komodo.shell.commands.core.CdCommand;

/**
 * Test Class to test CdCommand
 * 
 */
@SuppressWarnings("javadoc")
public class CdCommandTest extends AbstractCommandTest {

	private static final String CD_COMMAND1 = "cdCommand1.txt"; //$NON-NLS-1$
	private static final String CD_COMMAND2 = "cdCommand2.txt"; //$NON-NLS-1$
	
	/**
	 * Test for CdCommand
	 */
	public CdCommandTest( ) {
		super();
	}
	
	/**
     * Status at the workspace context root
	 * @throws Exception 
     */
    @Test
    public void testCd1() throws Exception {
    	setup(CD_COMMAND1, CdCommand.class);

    	execute();
    	
    	// Check WorkspaceContext
    	assertEquals("home.Project2", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }
    
	/**
     * Status at the workspace Project2 context
	 * @throws Exception 
     */
    @Test
    public void testCd2() throws Exception {
    	setup(CD_COMMAND2, CdCommand.class);
    	
    	execute();
    	
    	// Check WorkspaceContext
    	assertEquals("home.Project2.MyModel", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }
    
}
