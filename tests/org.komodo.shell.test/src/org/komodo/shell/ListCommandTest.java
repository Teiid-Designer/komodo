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
import org.komodo.shell.commands.core.ListCommand;

/**
 * Test Class to test ListCommand
 * 
 */
@SuppressWarnings("javadoc")
public class ListCommandTest extends AbstractCommandTest {

	private static final String LIST_COMMAND1 = "listCommand1.txt"; //$NON-NLS-1$
	private static final String LIST_COMMAND2 = "listCommand2.txt"; //$NON-NLS-1$
	private static final String INDENT = getIndentStr();
	
	/**
	 * Test for ListCommand
	 */
	public ListCommandTest( ) {
		super();
	}
	
	/**
     * Status at the workspace context root
	 * @throws Exception 
     */
    @Test
    public void testList1() throws Exception {
    	setup(LIST_COMMAND1, ListCommand.class);
    	
    	execute();
    	
    	String expectedOutput = INDENT+"Project1 [PROJECT]\n"+ //$NON-NLS-1$
    	                        INDENT+"Project2 [PROJECT]\n"; //$NON-NLS-1$
    	String writerOutput = getCommandOutput();
    	assertEquals(expectedOutput,writerOutput);
    	assertEquals("home", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }
    
	/**
     * Status at the workspace Project2 context
	 * @throws Exception 
     */
    @Test
    public void testList2() throws Exception {
    	setup(LIST_COMMAND2, ListCommand.class);
    	
    	execute();
    	
    	String expectedOutput = INDENT+"Model1 [MODEL]\n"+ //$NON-NLS-1$
    			                INDENT+"Model2 [MODEL]\n"+ //$NON-NLS-1$ 
    			                INDENT+"Model3 [MODEL]\n"; //$NON-NLS-1$ 
    	String writerOutput = getCommandOutput();
    	assertEquals(expectedOutput,writerOutput);
    	assertEquals("home.Project1", wsStatus.getCurrentContext().getFullName()); //$NON-NLS-1$
    }
    
}
