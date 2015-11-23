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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;

/**
 * Test Class to test entry of an invalid command.
 */
@SuppressWarnings({"javadoc", "nls"})
public class CommandNotFoundTest extends AbstractCommandTest {

    @Test
    public void shouldFailInvalidCommand() throws Exception {
        ShellCommand command = wsStatus.getCommand("bad-command");
        CommandResult result = command.execute();
        
        // CommandNotFound - make sure message contains bad command name
        assertFalse(result.isOk());
        assertTrue(result.getMessage().contains("bad-command"));
        assertTrue(result.getMessage().contains("not found"));
    }

}
