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

import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link ListCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class ListCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String[] commands = { "list extraArg" };
        execute( commands );
    }

    @Test
    public void testList1() throws Exception {
        final String[] commands =  { "workspace",
                                     "list" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        // workspace is empty
    	String writerOutput = getCommandOutput();
        assertTrue( writerOutput.contains( "no children" ) );
    }

    @Test
    public void testListLsAlias() throws Exception {
        final String[] commands =  { "workspace",
                                     "ls" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        // workspace is empty
        String writerOutput = getCommandOutput();
        assertTrue( writerOutput.contains( "no children" ) );
    }

    @Test
    public void testListLlAlias() throws Exception {
        final String[] commands =  { "workspace",
                                     "ll" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        // workspace is empty
        String writerOutput = getCommandOutput();
        assertTrue( writerOutput.contains( "no children" ) );
    }
    
    @Test
    public void testTabCompleter()throws Exception{
    	ArrayList<CharSequence> candidates=new ArrayList<>();
       	setup("commandFiles","addChildren.cmd");
       	assertTabCompletion("list invalid", candidates);
       	    
    	candidates.add("myChild1/");
    	candidates.add("myChild2/");
    	assertTabCompletion("list myCh", candidates);

    	candidates.add("MyChild3/");
    	candidates.add("..");
    	assertTabCompletion("list ", candidates);

    	candidates.clear();
    	candidates.add("myChild1/mySubChild1/");
    	candidates.add("myChild1/mySubChild2/");
    	assertTabCompletion("list myChild1/myS", candidates);

    	candidates.add("myChild1/MySubChild3/");
    	assertTabCompletion("list myChild1/", candidates);
    }

}
