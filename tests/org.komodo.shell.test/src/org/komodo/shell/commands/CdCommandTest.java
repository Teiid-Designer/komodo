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
package org.komodo.shell.commands;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;

/**
 * Test class for {@link CdCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class CdCommandTest extends AbstractCommandTest {

    @Test
    public void shouldCdAbsolute() throws Exception {
        final String[] commands = { "cd /workspace" };
    	setup( commands );

        CommandResult result = execute();
        assertCommandResultOk(result);

    	// Check WorkspaceContext
    	assertEquals("/workspace", wsStatus.getDisplayPath(wsStatus.getCurrentContext()));
    }

    @Test
    public void shouldCdRelative() throws Exception {
        final String[] commands = { "workspace",
                                    "cd .." };
    	setup( commands );

        CommandResult result = execute();
        assertCommandResultOk(result);

    	// Check WorkspaceContext
        String contextPath = wsStatus.getCurrentContextDisplayPath();
        assertEquals("/", contextPath);
    }

    @Test
    public void shouldTestGoToAlias() throws Exception {
        final String[] commands = { "goto /workspace" };
        setup( commands );

        CommandResult result = execute();
        assertCommandResultOk(result);

        String contextPath = wsStatus.getCurrentContextDisplayPath();
        assertEquals("/workspace", contextPath);
    }

    @Test
    public void shouldTestEditAlias() throws Exception {
        final String[] commands = { "edit /workspace" };
        setup( commands );

        CommandResult result = execute();
        assertCommandResultOk(result);

        String contextPath = wsStatus.getCurrentContextDisplayPath();
        assertEquals("/workspace", contextPath);
    }

}
