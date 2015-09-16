/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.relational.RestVdb;
import org.komodo.rest.relational.RestVdbDataRole;
import org.komodo.rest.relational.RestVdbEntry;
import org.komodo.rest.relational.RestVdbImport;
import org.komodo.rest.relational.RestVdbTranslator;
import org.komodo.spi.constants.StringConstants;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestVdbTest {

    private static final String DESCRIPTION = "my description";
    private static final String ORIGINAL_FILE = "/Users/ElvisIsKing/MyVdb.xml";
    private static final String VDB_NAME = "MyVdb";

    private RestVdb vdb;

    private RestVdb copy() {
        final RestVdb copy = new RestVdb( this.vdb.getName() );
        copy.setDescription( this.vdb.getDescription() );
        copy.setOriginalFilePath( this.vdb.getOriginalFilePath() );
        copy.setDataRoles( this.vdb.getDataRoles() );
        copy.setEntries( this.vdb.getEntries() );
        copy.setImports( this.vdb.getImports() );
        copy.setTranslators( this.vdb.getTranslators() );
        copy.setLinks( this.vdb.getLinks() );
        copy.setProperties( this.vdb.getProperties() );

        return copy;
    }

    @Before
    public void init() {
        this.vdb = new RestVdb( VDB_NAME );
        this.vdb.setDescription( DESCRIPTION );
        this.vdb.setOriginalFilePath( ORIGINAL_FILE );

        // data roles

        // entries

        // imports

        // translators
    }

    @Test
    public void shouldBeEqual() {
        final RestVdb thatVdb = copy();
        assertThat( this.vdb, is( thatVdb ) );
    }

    @Test
    public void shouldBeEqualWhenComparingEmptyVdbs() {
        final RestVdb empty1 = new RestVdb();
        final RestVdb empty2 = new RestVdb();
        assertThat( empty1, is( empty2 ) );
    }

    @Test
    public void shouldConstructEmptyVdb() {
        final RestVdb empty = new RestVdb();
        assertThat( empty.getDescription(), is( nullValue() ) );
        assertThat( empty.getName(), is( nullValue() ) );
        assertThat( empty.getOriginalFilePath(), is( nullValue() ) );
        assertThat( empty.getDataRoles().length, is( 0 ) );
        assertThat( empty.getEntries().length, is( 0 ) );
        assertThat( empty.getImports().length, is( 0 ) );
        assertThat( empty.getTranslators().length, is( 0 ) );
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
        final RestVdb thatVdb = copy();
        assertThat( this.vdb.hashCode(), is( thatVdb.hashCode() ) );
    }

    @Test
    public void shouldNotBeEqualWhenDataRolesAreDifferent() {
        final RestVdb thatVdb = copy();
        final RestVdbDataRole[] newDataRoles = new RestVdbDataRole[ 1 ];
        newDataRoles[ 0 ] = new RestVdbDataRole( "rocknrole" );
        thatVdb.setDataRoles( newDataRoles );
        assertThat( this.vdb, is( not( thatVdb ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenDescriptionIsDifferent() {
        final RestVdb thatVdb = copy();
        thatVdb.setDescription( this.vdb.getDescription() + "blah" );
        assertThat( this.vdb, is( not( thatVdb ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenEntriesAreDifferent() {
        final RestVdb thatVdb = copy();
        final RestVdbEntry[] newEntries = new RestVdbEntry[ 1 ];
        newEntries[ 0 ] = new RestVdbEntry( "foot", "ball" );
        thatVdb.setEntries( newEntries );
        assertThat( this.vdb, is( not( thatVdb ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenImportsAreDifferent() {
        final RestVdb thatVdb = copy();
        final RestVdbImport[] newImports = new RestVdbImport[ 1 ];
        newImports[ 0 ] = new RestVdbImport( "blah", 500 );
        thatVdb.setImports( newImports );
        assertThat( this.vdb, is( not( thatVdb ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenNameIsDifferent() {
        final RestVdb thatVdb = copy();
        thatVdb.setName( this.vdb.getName() + "blah" );
        assertThat( this.vdb, is( not( thatVdb ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenTranslatorsAreDifferent() {
        final RestVdb thatVdb = copy();
        final RestVdbTranslator[] newTranslators = new RestVdbTranslator[ 1 ];
        newTranslators[ 0 ] = new RestVdbTranslator( "base", "ball" );
        thatVdb.setTranslators( newTranslators );
        assertThat( this.vdb, is( not( thatVdb ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenOriginalFileIsDifferent() {
        final RestVdb thatVdb = copy();
        thatVdb.setOriginalFilePath( this.vdb.getOriginalFilePath() + "blah" );
        assertThat( this.vdb, is( not( thatVdb ) ) );
    }

    @Test
    public void shouldSetDataRoles() {
        final RestVdbDataRole[] newDataRoles = new RestVdbDataRole[ 1 ];
        newDataRoles[ 0 ] = new RestVdbDataRole( "rocknrole" );
        this.vdb.setDataRoles( newDataRoles );
        assertThat( this.vdb.getDataRoles().length, is( newDataRoles.length ) );
        assertThat( this.vdb.getDataRoles()[ 0 ], is( newDataRoles[ 0 ] ) );
    }

    @Test
    public void shouldSetDescription() {
        final String newDescription = "blah";
        this.vdb.setDescription( newDescription );
        assertThat( this.vdb.getDescription(), is( newDescription ) );
    }

    @Test
    public void shouldSetEntries() {
        final RestVdbEntry[] newEntries = new RestVdbEntry[ 1 ];
        newEntries[ 0 ] = new RestVdbEntry( "foot", "ball" );
        this.vdb.setEntries( newEntries );
        assertThat( this.vdb.getEntries().length, is( newEntries.length ) );
        assertThat( this.vdb.getEntries()[ 0 ], is( newEntries[ 0 ] ) );
    }

    @Test
    public void shouldSetImports() {
        final RestVdbImport[] newImports = new RestVdbImport[ 1 ];
        newImports[ 0 ] = new RestVdbImport( "blah", 500 );
        this.vdb.setImports( newImports );
        assertThat( this.vdb.getImports().length, is( newImports.length ) );
        assertThat( this.vdb.getImports()[ 0 ], is( newImports[ 0 ] ) );
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

    @Test
    public void shouldSetTranslators() {
        final RestVdbTranslator[] newTranslators = new RestVdbTranslator[ 1 ];
        newTranslators[ 0 ] = new RestVdbTranslator( "base", "ball" );
        this.vdb.setTranslators( newTranslators );
        assertThat( this.vdb.getTranslators().length, is( newTranslators.length ) );
        assertThat( this.vdb.getTranslators()[ 0 ], is( newTranslators[ 0 ] ) );
    }

}
