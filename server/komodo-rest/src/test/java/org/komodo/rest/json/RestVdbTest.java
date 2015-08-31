/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;
import org.komodo.spi.constants.StringConstants;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestVdbTest {

    private static final String DESCRIPTION = "my description";
    private static final String ORIGINAL_FILE = "/Users/ElvisIsKing/MyVdb.xml";
    private static final String VDB_NAME = "MyVdb";

    private RestVdb vdb;

    @Before
    public void init() {
        this.vdb = new RestVdb( VDB_NAME );
        this.vdb.setDescription( DESCRIPTION );
        this.vdb.setOriginalFilePath( ORIGINAL_FILE );
    }

    @Test
    public void shouldBeEqual() {
        final RestVdb thatVdb = new RestVdb( this.vdb.getName() );
        thatVdb.setDescription( this.vdb.getDescription() );
        thatVdb.setOriginalFilePath( this.vdb.getOriginalFilePath() );
        thatVdb.setLinks( this.vdb.getLinks() );
        thatVdb.setProperties( this.vdb.getProperties() );

        assertThat( this.vdb, is( thatVdb ) );
    }

    @Test
    public void shouldBeEqualWhenComparingEmptyVdbs() {
        final RestVdb empty1 = new RestVdb();
        final RestVdb empty2 = new RestVdb();
        assertThat( empty1, is( empty2 ) );
    }

    public void shouldConstructEmptyVdb() {
        final RestVdb empty = new RestVdb();
        assertThat( empty.getDescription(), is( nullValue() ) );
        assertThat( empty.getName(), is( nullValue() ) );
        assertThat( empty.getOriginalFilePath(), is( nullValue() ) );
        assertThat( empty.getProperties().isEmpty(), is( true ) );
        assertThat( empty.getLinks().length, is( 0 ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenVdbNameIsEmpty() {
        new RestVdb( StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenVdbNameIsNull() {
        new RestVdb( null );
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestVdb thatVdb = new RestVdb( this.vdb.getName() );
        thatVdb.setDescription( this.vdb.getDescription() );
        thatVdb.setOriginalFilePath( this.vdb.getOriginalFilePath() );
        assertThat( this.vdb.hashCode(), is( thatVdb.hashCode() ) );
    }

    @Test
    public void shouldNotBeEqualWhenDescriptionIsDifferent() {
        final RestVdb thatVdb = new RestVdb( this.vdb.getName() );
        thatVdb.setDescription( this.vdb.getDescription() + "blah" );
        thatVdb.setOriginalFilePath( this.vdb.getOriginalFilePath() );
        thatVdb.setLinks( this.vdb.getLinks() );
        thatVdb.setProperties( this.vdb.getProperties() );

        assertThat( this.vdb.getDescription(), is( not( thatVdb.getDescription() ) ) );
        assertThat( this.vdb, is( not( thatVdb ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenNameIsDifferent() {
        final RestVdb thatVdb = new RestVdb( this.vdb.getName() + "blah" );
        thatVdb.setDescription( this.vdb.getDescription() );
        thatVdb.setOriginalFilePath( this.vdb.getOriginalFilePath() );
        thatVdb.setLinks( this.vdb.getLinks() );
        thatVdb.setProperties( this.vdb.getProperties() );

        assertThat( this.vdb.getName(), is( not( thatVdb.getName() ) ) );
        assertThat( this.vdb, is( not( thatVdb ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenOriginalFileIsDifferent() {
        final RestVdb thatVdb = new RestVdb( this.vdb.getName() );
        thatVdb.setDescription( this.vdb.getDescription() );
        thatVdb.setOriginalFilePath( this.vdb.getOriginalFilePath() + "blah" );
        thatVdb.setLinks( this.vdb.getLinks() );
        thatVdb.setProperties( this.vdb.getProperties() );

        assertThat( this.vdb.getOriginalFilePath(), is( not( thatVdb.getOriginalFilePath() ) ) );
        assertThat( this.vdb, is( not( thatVdb ) ) );
    }

    @Test
    public void shouldSetDescription() {
        final String newDescription = "blah";
        this.vdb.setDescription( newDescription );
        assertThat( this.vdb.getDescription(), is( newDescription ) );
    }

    @Test
    public void shouldSetName() {
        final String newName = "blah";
        this.vdb.setName( newName );
        assertThat( this.vdb.getName(), is( newName ) );
    }

    @Test
    public void shouldSetOriginalFilePath() {
        final String newPath = "blah";
        this.vdb.setOriginalFilePath( newPath );
        assertThat( this.vdb.getOriginalFilePath(), is( newPath ) );
    }

}
