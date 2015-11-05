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

import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link ShowStatusCommand}.
 */
@SuppressWarnings({"javadoc", "nls"})
public class ShowStatusCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String[] commands = { "show-status extraArg" };
        execute( commands );
    }

    @Test
    public void testShowStatus1() throws Exception {
        final String[] commands = { "workspace",
                                    "show-status" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

    	// make sure repository URL and workspace appear, no Teiid is set, and current context path
    	String writerOutput = getCommandOutput();
        assertTrue(writerOutput.contains("test-local-repository-in-memory-config.json"));
        assertTrue(writerOutput.contains("Name : komodoLocalWorkspace"));
        assertTrue(writerOutput.contains("/workspace"));
    }

}
