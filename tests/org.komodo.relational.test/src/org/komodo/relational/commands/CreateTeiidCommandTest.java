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

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.komodo.relational.AbstractCommandTest;
import org.komodo.shell.util.KomodoObjectUtils;

/**
 * Test Class to test CreateTeiidCommand
 *
 */
@SuppressWarnings("javadoc")
public class CreateTeiidCommandTest extends AbstractCommandTest {

	private static final String CREATE_TEIID_COMMAND_1 = "createTeiidCommand_1.txt"; //$NON-NLS-1$

    /**
	 * Test for CreateTeiidCommand
	 */
	public CreateTeiidCommandTest( ) {
		super();
	}

    @Test
    public void testCreateTeiid1() throws Exception {
    	setup(CREATE_TEIID_COMMAND_1, CreateTeiidCommand.class);

    	execute();

    	// Check WorkspaceContext
    	assertEquals("/workspace", KomodoObjectUtils.getFullName(wsStatus, wsStatus.getCurrentContext())); //$NON-NLS-1$
    }

}
