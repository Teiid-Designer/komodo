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
package org.komodo.relational.commands;

import java.io.File;
import java.io.FileWriter;
import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.komodo.relational.AbstractCommandTest;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test UploadVdbCommand
 *
 */
@SuppressWarnings("javadoc")
public class UploadVdbCommandTest extends AbstractCommandTest {

    private static final String UPLOAD_VDB = "./resources/AzureService-vdb.xml"; //$NON-NLS-1$
    
    /**
	 * Test for UploadVdbCommand
	 */
	public UploadVdbCommandTest( ) {
		super();
	}

    @Test
    public void shouldUploadVdb1() throws Exception {
        File cmdFile = File.createTempFile("TestCommand", ".txt");  //$NON-NLS-1$  //$NON-NLS-2$
        cmdFile.deleteOnExit();
        
        FileWriter writer = new FileWriter(cmdFile);
        writer.write("workspace" + NEW_LINE);  //$NON-NLS-1$
        writer.write("upload-vdb myVdb " + UPLOAD_VDB + NEW_LINE); //$NON-NLS-1$ 
        writer.close();
        
    	setup(cmdFile.getAbsolutePath(), UploadVdbCommand.class);

        CommandResult result = execute();
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo);
        Vdb[] vdbs = wkspMgr.findVdbs(uow);
        
        assertEquals(1, vdbs.length);
        assertEquals("myVdb", vdbs[0].getName(uow)); //$NON-NLS-1$
    }

}
