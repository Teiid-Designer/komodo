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
 * Test Class to test {@link ShowDescriptorsCommand}.
 */
@SuppressWarnings({"javadoc", "nls"})
public class ShowDescriptorsCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String[] commands = { "show-descriptors extraArg" };
        setup( commands );
        execute();
    }
    
//    @Test
//    public void shouldShowRootDescriptors() throws Exception {
//        final String[] commands = { "show-descriptors" };
//        setup( commands );
//
//        CommandResult result = execute();
//        assertCommandResultOk(result);
//
//        // root primary type
//        String writerOutput = getCommandOutput();
//        assertTrue(writerOutput.contains("No Descriptors"));
//    }
//
//    @Test
//    public void shouldShowWorkspaceDescriptors() throws Exception {
//        final String[] commands = { 
//            "workspace",
//            "show-descriptors"};
//        setup( commands );
//
//        CommandResult result = execute();
//        assertCommandResultOk(result);
//
//        // root primary type
//        String writerOutput = getCommandOutput();
//        assertTrue(writerOutput.contains("No Descriptors"));
//    }

}
