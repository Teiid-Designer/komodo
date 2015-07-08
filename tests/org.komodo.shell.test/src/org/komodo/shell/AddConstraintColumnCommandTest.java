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
import static org.hamcrest.core.IsCollectionContaining.hasItems;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.junit.Assert.assertThat;
import java.util.Arrays;
import org.junit.Test;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.commands.core.AddConstraintColumnCommand;

/**
 * Test for the {@link AddConstraintColumnCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class AddConstraintColumnCommandTest extends AbstractCommandTest {

    private static final String ADD_COLUMNS_TO_PRIMARY_KEY = "addColumnsToPrimaryKey.txt"; //$NON-NLS-1$

    @Test
    public void shouldAddColumnReferencesToPrimaryKey() throws Exception {
        setup( ADD_COLUMNS_TO_PRIMARY_KEY, AddConstraintColumnCommand.class );

        execute();

        final WorkspaceContext context = this.wsStatus.getCurrentContext();
        assertThat( context.getFullName(), is( "/workspace/MyVdb/MyModel/MyTable/pk" ) );
        assertThat( context.getKomodoObj(), is( instanceOf( PrimaryKey.class ) ) );

        final PrimaryKey pk = ( PrimaryKey )context.getKomodoObj();
        final Column[] refCols = pk.getColumns( this.uow );
        assertThat( refCols.length, is( 2 ) );

        final String[] refColNames = new String[] { refCols[0].getName( this.uow ), refCols[1].getName( this.uow ) };
        assertThat( Arrays.asList( refColNames ), hasItems( "column_1", "column_2" ) );
    }

}
