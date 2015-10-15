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
package org.komodo.shell;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.commands.PlayCommand;

@SuppressWarnings( { "javadoc", "nls" } )
public final class PlayCommandTest extends AbstractCommandTest {

//    @Test
//    public void shouldPlayFile() throws Exception {
//        setup( "PartsVDBScript.txt", PlayCommand.class );
//        execute();
//        assertThat( this.wsStatus.getCurrentContextFullName(), is( "/workspace" ) );
//
//        final WorkspaceManager wsMgr = this.wsStatus.getCurrentContext().getWorkspaceManager();
//        final UnitOfWork transaction = this.wsStatus.getTransaction();
//        assertThat( wsMgr.findVdbs( transaction ).length, is( 1 ) );
//
//        final Vdb vdb = wsMgr.findVdbs( transaction )[ 0 ];
//        assertThat( vdb.getName( transaction ), is( "PartsVDB" ) );
//
//        { // 3 models
//            assertThat( Arrays.asList( wsMgr.findByType( transaction, VdbLexicon.Vdb.DECLARATIVE_MODEL ) ),
//                        hasItems( "/tko:komodo/tko:workspace/PartsVDB/PartsSQLServer",
//                                  "/tko:komodo/tko:workspace/PartsVDB/PartsOracle",
//                                  "/tko:komodo/tko:workspace/PartsVDB/PartsViews" ) );
//        }
//
//        { // 1 translator
//            assertThat( wsMgr.findByType( transaction, VdbLexicon.Translator.TRANSLATOR ).length, is( 1 ) );
//            assertThat( wsMgr.findByType( transaction, VdbLexicon.Translator.TRANSLATOR )[ 0 ],
//                        is( "/tko:komodo/tko:workspace/PartsVDB/vdb:translators/custom_oracle" ) );
//        }
//
//        { // 10 tables
//            assertThat( wsMgr.findByType( transaction, CreateTable.TABLE_STATEMENT ).length, is( 10 ) );
//            assertThat( Arrays.asList( wsMgr.findByType( transaction, CreateTable.TABLE_STATEMENT ) ),
//                        hasItems( "/tko:komodo/tko:workspace/PartsVDB/PartsSQLServer/PARTS",
//                                  "/tko:komodo/tko:workspace/PartsVDB/PartsSQLServer/SHIP_VIA",
//                                  "/tko:komodo/tko:workspace/PartsVDB/PartsSQLServer/STATUS",
//                                  "/tko:komodo/tko:workspace/PartsVDB/PartsSQLServer/SUPPLIER",
//                                  "/tko:komodo/tko:workspace/PartsVDB/PartsSQLServer/SUPPLIER_PARTS",
//                                  "/tko:komodo/tko:workspace/PartsVDB/PartsOracle/PARTS",
//                                  "/tko:komodo/tko:workspace/PartsVDB/PartsOracle/SHIP_VIA",
//                                  "/tko:komodo/tko:workspace/PartsVDB/PartsOracle/STATUS",
//                                  "/tko:komodo/tko:workspace/PartsVDB/PartsOracle/SUPPLIER",
//                                  "/tko:komodo/tko:workspace/PartsVDB/PartsOracle/SUPPLIER_PARTS" ) );
//        }
//
//        { // 1 view
//            assertThat( wsMgr.findByType( transaction, CreateTable.VIEW_STATEMENT ).length, is( 1 ) );
//            assertThat( wsMgr.findByType( transaction, CreateTable.VIEW_STATEMENT )[ 0 ],
//                        is( "/tko:komodo/tko:workspace/PartsVDB/PartsViews/PartSummary" ) );
//        }
//    }

    @Test
    public void shouldFailToPlayNonExistentFile() throws Exception {
        setup( "bogus.txt", PlayCommand.class );
        CommandResult result = execute();
        
        assertEquals(false, result.isOk());
        String msg = result.getMessage();
        assertEquals(true, msg.contains("Problem with File"));
    }

}
