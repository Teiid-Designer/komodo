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
 * Test Class to test {@link ListCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class ListCommandTest extends AbstractCommandTest {

    @Test
    public void testList1() throws Exception {
        final String[] commands =  { "workspace",
                                     "list" };
    	setup( commands );

        CommandResult result = execute();
        assertCommandResultOk(result);

        // workspace is empty
    	String writerOutput = getCommandOutput();
        assertTrue( writerOutput.contains( "no children" ) );
    }

    @Test
    public void testListLsAlias() throws Exception {
        final String[] commands =  { "workspace",
                                     "ls" };
        setup( commands );

        CommandResult result = execute();
        assertCommandResultOk(result);

        // workspace is empty
        String writerOutput = getCommandOutput();
        assertTrue( writerOutput.contains( "no children" ) );
    }

    @Test
    public void testListLlAlias() throws Exception {
        final String[] commands =  { "workspace",
                                     "ll" };
        setup( commands );

        CommandResult result = execute();
        assertCommandResultOk(result);

        // workspace is empty
        String writerOutput = getCommandOutput();
        assertTrue( writerOutput.contains( "no children" ) );
    }

}
