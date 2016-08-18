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
package org.komodo.relational.commands.server;

import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.repository.KomodoObject;

/**
 * Test Class to test {@link ServerSetPropertyCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class ServerSetPropertyCommandTest extends AbstractServerCommandTest {

    @Test
    public void testSetProperty1() throws Exception {
        final String[] commands = {
            "server-set-property adminPort 8765",
            "server-show-properties" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        // Get the teiid server, make sure the port is correct.
        List<KomodoObject> results = _repo.searchByType(sysTx(), KomodoLexicon.Teiid.NODE_TYPE);
        KomodoObject serverObj = results.get(0);
        if(Teiid.RESOLVER.resolvable(getTransaction(), serverObj)) {
            Teiid server = Teiid.RESOLVER.resolve(getTransaction(), serverObj);
            assertTrue(server.getAdminPort(getTransaction())==8765);
        }
    }

    @Test
    public void testTabCompleter()throws Exception{
    	ArrayList<CharSequence> candidates=new ArrayList<CharSequence>();
    	candidates.add(ServerShellCommand.ADMIN_PORT);
    	candidates.add(ServerShellCommand.ADMIN_PSWD);
    	assertTabCompletion("server-set-property ADMINP", candidates);
    	assertTabCompletion("server-set-property adminP", candidates);
    }

}
