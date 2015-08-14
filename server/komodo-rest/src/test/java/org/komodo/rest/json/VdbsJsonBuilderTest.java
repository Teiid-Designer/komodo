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
import javax.json.JsonArray;
import javax.json.JsonObject;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.repository.Repository.UnitOfWork;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbsJsonBuilderTest {

    private static JsonBuilderContext _context;

    @BeforeClass
    public static void constructContext() {
        _context = new JsonBuilderContext();
        _context.rootUri = "http://localhost:8080/";
    }

    private Vdb createVdb( final String vdbName,
                           final String description,
                           final UnitOfWork uow ) throws Exception {
        final Vdb vdb = mock( Vdb.class );
        when( vdb.getName( uow ) ).thenReturn( vdbName );
        when( vdb.getDescription( uow ) ).thenReturn( description );
        return vdb;
    }

    @Test
    public void shouldBuildVdbDescriptorDocument() throws Exception {
        final UnitOfWork uow = mock( UnitOfWork.class );
        final int count = 10;
        final Vdb[] vdbs = new Vdb[ count ];

        for ( int i = 0; i < count; ++i ) {
            final String description = ( "VDB " + i + "description goes here" );
            final String vdbName = ( "Vdb" + i );
            vdbs[ i ] = createVdb( vdbName, description, uow );
        }

        final JsonObject json = VdbsJsonBuilder.BUILDER.build( vdbs, uow, _context );
        final JsonArray vdbsJson = json.getJsonArray( "vdbs" );

        assertThat( vdbsJson, is( notNullValue() ) );
        assertThat( vdbsJson.size(), is( count ) );
        assertThat( vdbsJson.getJsonObject( 0 ).getString( JsonName.ID ), is( "Vdb0" ) );
        assertThat( vdbsJson.getJsonObject( count - 1 ).getString( JsonName.ID ), is( "Vdb" + ( count - 1 ) ) );
    }

}
