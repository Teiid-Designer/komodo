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
import static org.junit.Assert.assertNull;
import org.junit.Test;
import org.komodo.relational.AbstractCommandTest;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.repository.Property;

/**
 * Test Class to test UnsetCustomPropertyCommand
 *
 */
@SuppressWarnings("javadoc")
public class UnsetCustomPropertyCommandTest extends AbstractCommandTest {

    /**
	 * Test for UnsetCustomPropertyCommand
	 */
	public UnsetCustomPropertyCommandTest( ) {
		super();
	}

    @Test
    public void testUnsetCustomProperty1() throws Exception {
        File cmdFile = File.createTempFile("TestCommand", ".txt");  //$NON-NLS-1$  //$NON-NLS-2$
        cmdFile.deleteOnExit();
        
        FileWriter writer = new FileWriter(cmdFile);
        writer.write("workspace" + NEW_LINE);  //$NON-NLS-1$
        writer.write("create-vdb testVdb vdbPath" + NEW_LINE);  //$NON-NLS-1$
        writer.write("cd testVdb" + NEW_LINE);  //$NON-NLS-1$
        writer.write("set-custom-property newProperty myProperty" + NEW_LINE);  //$NON-NLS-1$
        writer.write("unset-custom-property newProperty" + NEW_LINE);  //$NON-NLS-1$
        writer.close();
        
    	setup(cmdFile.getAbsolutePath(), UnsetCustomPropertyCommand.class);

        CommandResult result = execute();
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo);
        Vdb[] vdbs = wkspMgr.findVdbs(uow);
        
        assertEquals(1, vdbs.length);
        assertEquals("testVdb", vdbs[0].getName(uow)); //$NON-NLS-1$

        Property prop = vdbs[0].getProperty(uow, "newProperty"); //$NON-NLS-1$
        assertNull(prop);
    }

}
