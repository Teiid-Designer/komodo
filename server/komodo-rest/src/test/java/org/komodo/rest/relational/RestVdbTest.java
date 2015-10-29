/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import static org.hamcrest.core.IsNot.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import java.net.URI;
import javax.ws.rs.core.UriBuilder;
import org.junit.Before;
import org.junit.Test;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;

@SuppressWarnings( {"javadoc", "nls"} )
public final class RestVdbTest {

    private static final URI BASE_URI = UriBuilder.fromUri("http://localhost:8081/v1/workspace/").build();
    private static final String VDB_NAME = "MyVdb";
    private static final String DATA_PATH = "/data/path";
    private static final KomodoType kType = KomodoType.VDB;
    private static final String DESCRIPTION = "my description";
    private static final String ORIGINAL_FILE = "/Users/ElvisIsKing/MyVdb.xml";
    private static final String CONNECTION_TYPE = "BY_VERSION";
    private static final int VERSION = 1;

    private RestVdb vdb;

    private RestVdb copy() {
        final RestVdb copy = new RestVdb(vdb.getBaseUri(), vdb.getName(), vdb.getDataPath(), vdb.getkType(), vdb.hasChildren());
        copy.setName(this.vdb.getName());
        copy.setDescription(this.vdb.getDescription());
        copy.setOriginalFilePath(this.vdb.getOriginalFilePath());
        copy.setConnectionType(this.vdb.getConnectionType());
        copy.setPreview(this.vdb.isPreview());
        copy.setVersion(this.vdb.getVersion());
        copy.setLinks(this.vdb.getLinks());
        copy.setProperties(this.vdb.getProperties());

        return copy;
    }

    @Before
    public void init() {
        this.vdb = new RestVdb(BASE_URI, VDB_NAME, DATA_PATH, kType, true);
        this.vdb.setName(VDB_NAME);
        this.vdb.setDescription(DESCRIPTION);
        this.vdb.setOriginalFilePath(ORIGINAL_FILE);
        this.vdb.setConnectionType(CONNECTION_TYPE);
        this.vdb.setPreview(false);
        this.vdb.setVersion(VERSION);
    }

    @Test
    public void shouldBeEqual() {
        final RestVdb thatVdb = copy();
        assertEquals(this.vdb, thatVdb);
    }

    @Test
    public void shouldBeEqualWhenComparingEmptyVdbs() {
        final RestVdb empty1 = new RestVdb();
        final RestVdb empty2 = new RestVdb();
        assertEquals(empty1, empty2);
    }

    @Test
    public void shouldConstructEmptyVdb() {
        final RestVdb empty = new RestVdb();
        assertNull(empty.getBaseUri());
        assertNull(empty.getName());
        assertNull(empty.getDescription());
        assertNull(empty.getOriginalFilePath());
        assertEquals(empty.getProperties().isEmpty(), true);
        assertEquals(empty.getLinks().size(), 0);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenVdbNameIsEmpty() {
        new RestVdb(BASE_URI, StringConstants.EMPTY_STRING, DATA_PATH, kType, true);
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenVdbNameIsNull() {
        new RestVdb(BASE_URI, null, DATA_PATH, kType, true);
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestVdb thatVdb = copy();
        assertEquals(this.vdb.hashCode(), thatVdb.hashCode());
    }

    @Test
    public void shouldNotBeEqualWhenDescriptionIsDifferent() {
        final RestVdb thatVdb = copy();
        thatVdb.setDescription(this.vdb.getDescription() + "blah");
        assertNotEquals(this.vdb, not(thatVdb));
    }

    @Test
    public void shouldNotBeEqualWhenNameIsDifferent() {
        final RestVdb thatVdb = copy();
        thatVdb.setName(this.vdb.getName() + "blah");
        assertNotEquals(this.vdb, thatVdb);
    }

    @Test
    public void shouldNotBeEqualWhenOriginalFileIsDifferent() {
        final RestVdb thatVdb = copy();
        thatVdb.setOriginalFilePath(this.vdb.getOriginalFilePath() + "blah");
        assertNotEquals(this.vdb, thatVdb);
    }

    @Test
    public void shouldSetDescription() {
        final String newDescription = "blah";
        this.vdb.setDescription(newDescription);
        assertEquals(this.vdb.getDescription(), newDescription);
    }

    @Test
    public void shouldSetName() {
        final String newName = "blah";
        this.vdb.setName(newName);
        assertEquals(this.vdb.getName(), newName);
    }

    @Test
    public void shouldSetOriginalFilePath() {
        final String newPath = "blah";
        this.vdb.setOriginalFilePath(newPath);
        assertEquals(this.vdb.getOriginalFilePath(), newPath);
    }

}
