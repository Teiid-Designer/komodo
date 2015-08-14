/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import javax.json.JsonObject;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.repository.Repository;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbDescriptorJsonBuilderTest {

    private static JsonBuilderContext _context;

    @BeforeClass
    public static void constructContext() {
        _context = new JsonBuilderContext();
        _context.rootUri = "http://localhost:8080/";
    }

    @Test
    public void shouldBuildVdbsDocument() throws Exception {
        final Repository.UnitOfWork uow = mock( Repository.UnitOfWork.class );
        final Vdb vdb = mock( Vdb.class );
        final String description = "VDB description goes here";
        final String vdbName = "MyVdb";
        when( vdb.getName( uow ) ).thenReturn( vdbName );
        when( vdb.getDescription( uow ) ).thenReturn( description );

        final JsonObject json = VdbDescriptorJsonBuilder.BUILDER.build( vdb, uow, _context );
        assertThat( json.getString( JsonName.ID ), is( vdbName ) );
        assertThat( json.getString( JsonName.DESCRIPTION ), is( description ) );
        assertThat( json.getJsonArray( JsonName.LINKS ), is( notNullValue() ) );
        assertThat( json.getJsonArray( JsonName.LINKS ).size(), is( 2 ) );
        assertThat( json.getJsonArray( JsonName.LINKS ).getJsonObject( 0 ).getString( JsonName.REL ), is( JsonName.SELF ) );
        assertThat( json.getJsonArray( JsonName.LINKS ).getJsonObject( 1 ).getString( JsonName.REL ), is( JsonName.VIEW ) );
    }

}
