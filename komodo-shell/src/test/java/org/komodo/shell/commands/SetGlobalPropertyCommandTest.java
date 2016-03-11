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

import java.util.LinkedList;
import java.util.List;

import org.junit.Test;
import org.komodo.shell.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * Test Class to test {@link SetGlobalPropertyCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class SetGlobalPropertyCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String[] commands = { "set-global anArg extraArg" };
        execute( commands );
    }

    @Test( expected = AssertionError.class )
    public void shouldFailBadGlobalProperty( ) throws Exception {
        final String[] commands = { "set-global anArg" };
        execute( commands );
    }

    @Test
    public void shouldSetShowTypeInPrompt() throws Exception {
        final String[] commands = { "set-global " + WorkspaceStatus.SHOW_TYPE_IN_PROMPT_KEY + " true" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        // Check Context and property value
        assertEquals("/", wsStatus.getCurrentContextDisplayPath(null));
        assertEquals("true", wsStatus.getGlobalProperties(false).getProperty(WorkspaceStatus.SHOW_TYPE_IN_PROMPT_KEY));
    }

    @Test
    public void shouldSetRecordingOutputFile() throws Exception {
        final String[] commands = { "set-global " + WorkspaceStatus.RECORDING_FILE_KEY + " /aRecordingFile.txt" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        // Check Context and property value
        assertEquals("/", wsStatus.getCurrentContextDisplayPath(null));
        assertEquals("/aRecordingFile.txt", wsStatus.getGlobalProperties(false).getProperty(WorkspaceStatus.RECORDING_FILE_KEY));
    }

    @Test
    public void testTabCompleter()throws Exception{

        List<String> defaultValues = new LinkedList<>(WorkspaceStatus.GLOBAL_PROPS.keySet());
        assertTabCompletion("set-global ", defaultValues);

        defaultValues.clear();
        String showTypeParam=WorkspaceStatus.SHOW_TYPE_IN_PROMPT_KEY;
        defaultValues.add(showTypeParam);
        assertTabCompletion("set-global "+showTypeParam.substring(0, showTypeParam.length()-3), defaultValues);

    }

}
