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
import java.util.ArrayList;
import org.junit.Test;
import org.komodo.repository.RepositoryImpl;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;

/**
 * Test class for {@link CdCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class CdCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String[] commands = { "cd somewhere extraArg" };
        execute( commands );
    }

    @Test
    public void shouldCdAbsolute() throws Exception {
        final String[] commands = { "cd /workspace" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );
        assertContextIs(RepositoryImpl.komodoWorkspacePath(null));
    }

    @Test
    public void shouldCdRelative() throws Exception {
        final String[] commands = { "workspace",
                                    "cd .." };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

    	// Check WorkspaceContext
        String contextPath = wsStatus.getCurrentContextDisplayPath(null);
        assertEquals("/workspace", contextPath);
    }

    @Test
    public void shouldTestGoToAlias() throws Exception {
        final String[] commands = { "goto /workspace" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        String contextPath = wsStatus.getCurrentContextDisplayPath(null);
        assertEquals("/workspace", contextPath);
    }

    @Test
    public void shouldTestEditAlias() throws Exception {
        final String[] commands = { "edit /workspace" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        String contextPath = wsStatus.getCurrentContextDisplayPath(null);
        assertEquals("/workspace", contextPath);
    }

    @Test
    public void testTabCompleter()throws Exception{
    	ArrayList<CharSequence> candidates=new ArrayList<>();
       	setup("commandFiles","addChildren.cmd");
       	assertTabCompletion("cd invalid", candidates);

    	candidates.add("myChild1/");
    	candidates.add("myChild2/");
    	assertTabCompletion("cd myCh", candidates);

    	candidates.add("MyChild3/");
    	candidates.add("..");
    	assertTabCompletion("cd ", candidates);

    	candidates.clear();
    	candidates.add("myChild1/mySubChild1/");
    	candidates.add("myChild1/mySubChild2/");
    	assertTabCompletion("cd myChild1/myS", candidates);

    	candidates.add("myChild1/MySubChild3/");
    	assertTabCompletion("cd myChild1/", candidates);
    }

}
