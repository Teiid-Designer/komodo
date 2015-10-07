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
import org.komodo.shell.commands.HomeCommand;

/**
 * Test Class to test ListCommand
 *
 */
@SuppressWarnings({"javadoc", "nls"})
public class HomeCommandTest extends AbstractCommandTest {

	private static final String HOME_COMMAND1 = "homeCommand1.txt"; //$NON-NLS-1$

	/**
	 * Test for ListCommand
	 */
	public HomeCommandTest( ) {
		super();
	}

    @Test
    public void testHome1() throws Exception {
    	setup(HOME_COMMAND1, HomeCommand.class);

    	execute();

        // make sure no children and path appear in output
    	String writerOutput = getCommandOutput();
        assertTrue( writerOutput.contains( "no children" ) );
        assertTrue( writerOutput.contains( "Workspace \"/workspace\"" ) );

        //assertEquals("/workspace", wsStatus.getCurrentContextFullName()); //$NON-NLS-1$
    }

}
