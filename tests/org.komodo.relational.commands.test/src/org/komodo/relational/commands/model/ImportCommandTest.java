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
package org.komodo.relational.commands.model;

import java.io.File;
import java.io.FileWriter;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.util.KomodoObjectUtils;

/**
 * Test Class to test ImportCommand
 *
 */
@SuppressWarnings("javadoc")
public class ImportCommandTest extends AbstractCommandTest {

    /**
	 * Test for ImportCommand
	 */
	public ImportCommandTest( ) {
		super();
	}

    @Test
    public void testImportDdl1() throws Exception {
        File cmdFile = File.createTempFile("TestCommand", ".txt");  //$NON-NLS-1$  //$NON-NLS-2$
        cmdFile.deleteOnExit();
        
        FileWriter writer = new FileWriter(cmdFile);
        writer.write("workspace" + NEW_LINE);  //$NON-NLS-1$
        writer.write("create-vdb myVdb vdbPath" + NEW_LINE);  //$NON-NLS-1$
        writer.write("cd myVdb" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-model myModel " + NEW_LINE);  //$NON-NLS-1$
        writer.write("cd myModel" + NEW_LINE);  //$NON-NLS-1$
        writer.close();
        
        setup(cmdFile.getAbsolutePath(), ImportCommand.class);

        CommandResult result = execute();
        assertCommandResultOk(result);

    	// Check WorkspaceContext
    	assertEquals("/workspace", KomodoObjectUtils.getFullName(wsStatus, wsStatus.getCurrentContext())); //$NON-NLS-1$
    }

}
