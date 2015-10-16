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
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.UserDefinedFunction;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test DeleteUserDefinedFunctionCommand
 *
 */
@SuppressWarnings("javadoc")
public class DeleteUserDefinedFunctionCommandTest extends AbstractCommandTest {

    /**
	 * Test for DeleteUserDefinedFunctionCommand
	 */
	public DeleteUserDefinedFunctionCommandTest( ) {
		super();
	}

    @Test
    public void testDelete1() throws Exception {
        File cmdFile = File.createTempFile("TestCommand", ".txt");  //$NON-NLS-1$  //$NON-NLS-2$
        cmdFile.deleteOnExit();
        
        FileWriter writer = new FileWriter(cmdFile);
        writer.write("workspace" + NEW_LINE);  //$NON-NLS-1$
        writer.write("create-vdb myVdb vdbPath" + NEW_LINE);  //$NON-NLS-1$
        writer.write("cd myVdb" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-model myModel " + NEW_LINE);  //$NON-NLS-1$
        writer.write("cd myModel" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-user-defined-function myUserDefinedFunction1" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-user-defined-function myUserDefinedFunction2" + NEW_LINE);  //$NON-NLS-1$
        writer.write("delete-user-defined-function myUserDefinedFunction1" + NEW_LINE);  //$NON-NLS-1$
        writer.close();
        
        setup(cmdFile.getAbsolutePath(), DeleteUserDefinedFunctionCommand.class);

        CommandResult result = execute();
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo);
        Vdb[] vdbs = wkspMgr.findVdbs(uow);
        
        assertEquals(1, vdbs.length);
        
        Model[] models = vdbs[0].getModels(uow);
        assertEquals(1, models.length);
        assertEquals("myModel", models[0].getName(uow)); //$NON-NLS-1$
        
        Function[] functions = models[0].getFunctions(uow);
        assertEquals(1, functions.length);
        assertEquals(true, functions[0] instanceof UserDefinedFunction);
        assertEquals("myUserDefinedFunction2", functions[0].getName(uow)); //$NON-NLS-1$
    }

}
