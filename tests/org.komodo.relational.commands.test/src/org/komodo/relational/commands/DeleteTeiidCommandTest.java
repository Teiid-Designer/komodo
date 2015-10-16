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
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test DeleteTeiidCommand
 *
 */
@SuppressWarnings("javadoc")
public class DeleteTeiidCommandTest extends AbstractCommandTest {

    /**
	 * Test for DeleteTeiidCommand
	 */
	public DeleteTeiidCommandTest( ) {
		super();
	}

    @Test
    public void testDeleteTeiid1() throws Exception {
        File cmdFile = File.createTempFile("TestCommand", ".txt");  //$NON-NLS-1$  //$NON-NLS-2$
        cmdFile.deleteOnExit();
        
        FileWriter writer = new FileWriter(cmdFile);
        writer.write("set-auto-commit false" + NEW_LINE);  //$NON-NLS-1$
        writer.write("workspace" + NEW_LINE);  //$NON-NLS-1$
        writer.write("create-teiid testTeiid1" + NEW_LINE);  //$NON-NLS-1$
        writer.write("create-teiid testTeiid2" + NEW_LINE);  //$NON-NLS-1$
        writer.write("commit" + NEW_LINE);  //$NON-NLS-1$
        writer.write("delete-teiid testTeiid1" + NEW_LINE);  //$NON-NLS-1$
        writer.write("commit" + NEW_LINE);  //$NON-NLS-1$
        writer.close();
        
    	setup(cmdFile.getAbsolutePath(), DeleteTeiidCommand.class);

        CommandResult result = execute();
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo);
        Teiid[] teiids = wkspMgr.findTeiids(uow);
        
        assertEquals(1,teiids.length);
        assertEquals("testTeiid2",teiids[0].getName(uow)); //$NON-NLS-1$
    }

}
