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
package org.komodo.relational.commands.server;

import static org.junit.Assert.assertEquals;
import java.io.File;
import java.io.FileWriter;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test ServerDisconnectCommand
 *
 */
@SuppressWarnings({"javadoc", "nls"})
public class ServerDisconnectCommandTest extends AbstractCommandTest {

    /**
	 * Test for ServerDisconnectCommand
	 */
	public ServerDisconnectCommandTest( ) {
		super();
	}

    @Test
    public void shouldFailNoServerConnected() throws Exception {
        File cmdFile = File.createTempFile("TestCommand", ".txt");  
        cmdFile.deleteOnExit();
        
        FileWriter writer = new FileWriter(cmdFile);
        writer.write("set-auto-commit false" + NEW_LINE);  //$NON-NLS-1$
        writer.write("workspace" + NEW_LINE);  
        writer.write("create-teiid myTeiid" + NEW_LINE);  
        writer.write("commit" + NEW_LINE);  //$NON-NLS-1$
        writer.write("set-server myTeiid" + NEW_LINE);  
        writer.write("server-disconnect" + NEW_LINE);  //$NON-NLS-1$
        writer.close();

        setup(cmdFile.getAbsolutePath(), ServerSetCommand.class);

        CommandResult result = execute();
        String msg = result.getMessage();
        
        assertEquals("something", msg);
    }

}
