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
 * Test Class to test {@link ShowChildrenCommand}.
 */
@SuppressWarnings({"javadoc", "nls"})
public class ShowChildrenCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String[] commands = { "show-children extraArg" };
        execute( commands );
    }

    @Test
    public void shouldShowChildrenNoChildren() throws Exception {
        final String[] commands = { "workspace",
                                    "show-children" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        String writerOutput = getCommandOutput();
        assertTrue(writerOutput.contains("There are no children"));
    }

    @Test
    public void shouldShowTwoChildren() throws Exception {
        final String child1Name = "blah1";
        final String child2Name = "blah2";
        final String[] commands = {
            "workspace",
            "add-child " + child1Name,
            "add-child " + child2Name,
            "show-children" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        String writerOutput = getCommandOutput();
        assertTrue(writerOutput.contains(child1Name));
        assertTrue(writerOutput.contains(child2Name));
    }

}
