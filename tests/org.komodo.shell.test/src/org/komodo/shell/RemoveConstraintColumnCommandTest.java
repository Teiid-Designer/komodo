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

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsCollectionContaining.hasItem;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.junit.Assert.assertThat;
import java.util.Arrays;
import org.junit.Test;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.commands.core.RemoveConstraintColumnCommand;

/**
 * Test for the {@link RemoveConstraintColumnCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class RemoveConstraintColumnCommandTest extends AbstractCommandTest {

    private static final String REMOVE_COLUMN_FLOM_PRIMARY_KEY = "removeColumnFromPrimaryKey.txt"; //$NON-NLS-1$

    @Test
    public void shouldRemoveColumnReferenceFromPrimaryKey() throws Exception {
        setup( REMOVE_COLUMN_FLOM_PRIMARY_KEY, RemoveConstraintColumnCommand.class );

        execute();

        final WorkspaceContext context = this.wsStatus.getCurrentContext();
        assertThat( context.getFullName(), is( "/workspace/MyVdb/MyModel/MyTable/pk" ) );
        assertThat( context.getKomodoObj(), is( instanceOf( PrimaryKey.class ) ) );

        final PrimaryKey pk = ( PrimaryKey )context.getKomodoObj();
        final Column[] refCols = pk.getColumns( this.uow );
        assertThat( refCols.length, is( 1 ) );

        final String[] refColNames = new String[] { refCols[0].getName( this.uow ) };
        assertThat( Arrays.asList( refColNames ), hasItem( "column_2" ) );
    }

}
