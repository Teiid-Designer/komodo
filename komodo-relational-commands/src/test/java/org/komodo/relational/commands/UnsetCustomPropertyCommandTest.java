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
package org.komodo.relational.commands;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.repository.Property;

/**
 * Test Class to test {@link UnsetCustomPropertyCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class UnsetCustomPropertyCommandTest extends AbstractCommandTest {

    @Test
    public void testUnsetCustomProperty1() throws Exception {
        final String[] commands = { "create-vdb testVdb vdbPath",
                                    "cd testVdb",
                                    "set-custom-property newProperty myProperty",
                                    "unset-custom-property newProperty" };

        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());

        assertEquals(1, vdbs.length);
        assertEquals("testVdb", vdbs[0].getName(getTransaction()));

        Property prop = vdbs[0].getProperty(getTransaction(), "newProperty");
        assertNull(prop);
    }

}
