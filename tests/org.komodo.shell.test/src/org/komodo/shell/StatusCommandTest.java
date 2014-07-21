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
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.core.StatusCommand;

/**
 * Test Class to test StatusCommand
 * 
 */
public class StatusCommandTest extends AbstractCommandTest {

	private static final String STATUS_COMMANDS1 = "statusCommands1.txt"; //$NON-NLS-1$
	private static final String STATUS_COMMANDS2 = "statusCommands2.txt"; //$NON-NLS-1$
	private static final String STATUS_COMMANDS3 = "statusCommands3.txt"; //$NON-NLS-1$
	private static final String INDENT = getIndentStr();
	
	/**
	 * Test for StatusCommand
	 */
	public StatusCommandTest( ) {
		super();
	}
	
	/**
     * Status at the workspace context root
     */
    @Test
    public void testStatus1() {
    	WorkspaceStatus wsStatus = new WorkspaceStatusImpl(System.in, System.out);
    	setup(STATUS_COMMANDS1, StatusCommand.class, wsStatus);
    	
    	execute();
    	
    	String expectedOutput = INDENT+"Current Repo    : local Repository\n"+ //$NON-NLS-1$
                                INDENT+"Current Server  : [Unknown : not connected]\n"+ //$NON-NLS-1$
                                INDENT+"Current Context : [home]\n"; //$NON-NLS-1$
    	String writerOutput = getCommandOutput();
    	assertEquals(expectedOutput,writerOutput);
    	assertEquals("home", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	
    	teardown();
    }
    
	/**
     * Status at the workspace Project2 context
     */
    @Test
    public void testStatus2() {
    	WorkspaceStatus wsStatus = new WorkspaceStatusImpl(System.in, System.out);
    	setup(STATUS_COMMANDS2, StatusCommand.class, wsStatus);
    	
    	execute();
    	
    	String expectedOutput = INDENT+"Current Repo    : local Repository\n"+ //$NON-NLS-1$
    	                        INDENT+"Current Server  : [Unknown : not connected]\n"+ //$NON-NLS-1$
    	                        INDENT+"Current Context : [home.Project2]\n"; //$NON-NLS-1$
    	String writerOutput = getCommandOutput();
    	assertEquals(expectedOutput,writerOutput);
    	assertEquals("home.Project2", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	
    	teardown();
    }
    
	/**
     * Status at the workspace ViewModel1 context
     */
    @Test
    public void testStatus3() {
    	WorkspaceStatus wsStatus = new WorkspaceStatusImpl(System.in, System.out);
    	setup(STATUS_COMMANDS3, StatusCommand.class, wsStatus);

    	execute();
    	
    	String expectedOutput = INDENT+"Current Repo    : local Repository\n"+ //$NON-NLS-1$
                                INDENT+"Current Server  : [Unknown : not connected]\n"+ //$NON-NLS-1$
                                INDENT+"Current Context : [home.Project1.Model2]\n"; //$NON-NLS-1$
    	String writerOutput = getCommandOutput();
    	assertEquals(expectedOutput,writerOutput);
    	assertEquals("home.Project1.Model2", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
   	
    	teardown();
    }
   	
}
