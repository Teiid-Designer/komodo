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

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link HelpCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class HelpCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String[] commands = { "help cd extraArg" };
        execute( commands );
    }

    @Test
    public void shouldShowHelpAtContext() throws Exception {
        final String[] commands = { "workspace",
                                    "help" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        // check help text
        String writerOutput = getCommandOutput();
        assertTrue(writerOutput,
                   writerOutput.contains("The following commands are supported at this context"));
    }

    @Test
    public void shouldShowHelpForCommand() throws Exception {
        final String[] commands = { "workspace",
                                    "help cd" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        // check help text
        String writerOutput = getCommandOutput();
        assertTrue(writerOutput.contains("cd - change the current workspace context"));
    }

    @Test
    public void testTabCompleter()throws Exception{
    	ArrayList<CharSequence> candidates=new ArrayList<>();


    	assertTabCompletion("help show-S", candidates);

    	candidates.add("show-status");
    	candidates.add("show-summary");
    	assertTabCompletion("help show-s", candidates);

    	assertTabCompletion("help ", Arrays.asList(wsStatus.getAvailableCommandNames()));
    	
    }
}
