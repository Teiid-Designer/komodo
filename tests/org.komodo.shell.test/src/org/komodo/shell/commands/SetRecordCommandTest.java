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
 * Test Class to test {@link SetRecordCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class SetRecordCommandTest extends AbstractCommandTest {

    @Test( expected = AssertionError.class )
    public void shouldFailTooManyArgs( ) throws Exception {
        final String[] commands = { "set-record on extraArg" };
        setup( commands );
        execute();
    }
    
    @Test
    public void shouldSetRecordOn() throws Exception {
        final String[] commands = { "workspace",
                                    "set-record on" };
    	setup( commands );

        execute();

        assertEquals(true, wsStatus.getRecordingStatus());
    }

    @Test
    public void shouldSetRecordOff() throws Exception {
        final String[] commands = { "workspace",
                                    "set-record on",
                                    "set-record off" };
        setup( commands );

        CommandResult result = execute();
        assertCommandResultOk(result);

        assertEquals(false, wsStatus.getRecordingStatus());
    }

}
