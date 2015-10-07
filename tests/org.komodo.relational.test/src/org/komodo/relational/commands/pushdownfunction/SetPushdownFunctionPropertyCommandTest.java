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
package org.komodo.relational.commands.pushdownfunction;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.komodo.relational.AbstractCommandTest;
import org.komodo.shell.util.KomodoObjectUtils;

/**
 * Test Class to test SetPushdownFunctionPropertyCommand
 *
 */
@SuppressWarnings("javadoc")
public class SetPushdownFunctionPropertyCommandTest extends AbstractCommandTest {

	private static final String SET_PUSHDOWN_FUNCTION_PROPERTY_COMMAND_1 = "setPushdownFunctionPropertyCommand_1.txt"; //$NON-NLS-1$

    /**
	 * Test for SetPushdownFunctionPropertyCommand
	 */
	public SetPushdownFunctionPropertyCommandTest( ) {
		super();
	}

    @Test
    public void testSetProperty1() throws Exception {
        setup(SET_PUSHDOWN_FUNCTION_PROPERTY_COMMAND_1, SetPushdownFunctionPropertyCommand.class);

    	execute();

    	// Check WorkspaceContext
    	assertEquals("/workspace", KomodoObjectUtils.getFullName(wsStatus, wsStatus.getCurrentContext())); //$NON-NLS-1$
    }

}
