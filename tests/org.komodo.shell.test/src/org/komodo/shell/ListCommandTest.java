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

/**
 * Test Class to test ListCommand
 * 
 */
public class ListCommandTest extends AbstractCommandTest {

	private static final String LIST_COMMAND1 = "listCommand1.txt"; //$NON-NLS-1$
	private static final String LIST_COMMAND2 = "listCommand2.txt"; //$NON-NLS-1$
	
	/**
	 * Test for ListCommand
	 */
	public ListCommandTest( ) {
		super();
	}
	
	/**
     * Status at the workspace context root
     */
    @Test
    public void testList1() {
    	WorkspaceStatus wsStatus = new WorkpaceStatusImpl();
    	setup(LIST_COMMAND1,wsStatus);
    	
    	execute();
    	
    	String expectedOutput = "Project1 [PROJECT]\nProject2 [PROJECT]\n"; //$NON-NLS-1$
    	String writerOutput = getWriterOutput();
    	assertEquals(expectedOutput,writerOutput);
    	assertEquals("root", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	
    	teardown();
    }
    
	/**
     * Status at the workspace Project2 context
     */
    @Test
    public void testList2() {
    	WorkspaceStatus wsStatus = new WorkpaceStatusImpl();
    	setup(LIST_COMMAND2,wsStatus);
    	
    	execute();
    	
    	String expectedOutput = "SrcModel1 [SOURCE_MODEL]\nSrcModel2 [SOURCE_MODEL]\nViewModel1 [VIEW_MODEL]\n"; //$NON-NLS-1$
    	String writerOutput = getWriterOutput();
    	assertEquals(expectedOutput,writerOutput);
    	assertEquals("root.Project1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    	
    	teardown();
    }
    
}
