/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.rest.relational;

import static org.hamcrest.core.IsNot.not;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import java.net.URI;
import java.util.Collection;
import javax.ws.rs.core.UriBuilder;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.folder.Folder;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestLink;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.mockito.Mockito;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class RestVdbTest implements StringConstants {

    private static final URI BASE_URI = UriBuilder.fromUri("http://localhost:8081/v1/").build();
    private static final URI BASE_TEIID_URI = UriBuilder.fromUri("http://localhost:8081/v1/teiid").build();
    private static final String WORKSPACE_DATA_PATH = "/workspace";
    private static final String VDB_NAME = "MyVdb";
    private static final String VDB_DATA_PATH = "/workspace/vdbs/vdb1";
    private static final KomodoType kType = KomodoType.VDB;
    private static final String DESCRIPTION = "my description";
    private static final String ORIGINAL_FILE = "/Users/ElvisIsKing/MyVdb.xml";
    private static final String CONNECTION_TYPE = "BY_VERSION";
    private static final int VERSION = 1;

    private RestVdb vdb;

    private RestVdb copy() {
        final RestVdb copy = new RestVdb();

        copy.setBaseUri(vdb.getBaseUri());
        copy.setId(vdb.getName());
        copy.setDataPath(vdb.getDataPath());
        copy.setkType(vdb.getkType());
        copy.setHasChildren(vdb.hasChildren());
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
    public void init() throws Exception {
        UnitOfWork transaction = Mockito.mock(UnitOfWork.class);

        KomodoObject workspace = Mockito.mock(KomodoObject.class);
        Mockito.when(workspace.getAbsolutePath()).thenReturn(WORKSPACE_DATA_PATH);

        Descriptor vdbType = Mockito.mock(Descriptor.class);
        when(vdbType.getName()).thenReturn(VdbLexicon.Vdb.VIRTUAL_DATABASE);

        Vdb theVdb = Mockito.mock(Vdb.class);
        Mockito.when(theVdb.getPrimaryType(transaction)).thenReturn(vdbType);
        Mockito.when(theVdb.getName(transaction)).thenReturn(VDB_NAME);
        Mockito.when(theVdb.getAbsolutePath()).thenReturn(VDB_DATA_PATH);
        Mockito.when(theVdb.getTypeIdentifier(transaction)).thenReturn(kType);
        Mockito.when(theVdb.hasChildren(transaction)).thenReturn(true);
        Mockito.when(theVdb.getPropertyNames(transaction)).thenReturn(new String[0]);
        Mockito.when(theVdb.getPropertyDescriptors(transaction)).thenReturn(new PropertyDescriptor[0]);
        Mockito.when(theVdb.getParent(transaction)).thenReturn(workspace);

        this.vdb = new RestVdb(BASE_URI, theVdb, false, transaction);
        this.vdb.setName(VDB_NAME);
        this.vdb.setDescription(DESCRIPTION);
        this.vdb.setOriginalFilePath(ORIGINAL_FILE);
        this.vdb.setConnectionType(CONNECTION_TYPE);
        this.vdb.setPreview(false);
        this.vdb.setVersion(VERSION);
    }

    @Test
    public void shouldHaveBaseUri() {
        assertEquals(BASE_URI, this.vdb.getBaseUri());
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
        assertEquals(empty.getProperties().isEmpty(), true);
        assertEquals(empty.getLinks().size(), 0);
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

    @SuppressWarnings( "incomplete-switch" )
    @Test
    public void shouldReturnACachedTeiidRestVdb() throws Exception {

        String parentDataPath = "/tko:komodo/tko:workspace/tko:teiidCache/localhost/";
        UnitOfWork transaction = Mockito.mock(UnitOfWork.class);

        Descriptor teiidType = Mockito.mock(Descriptor.class);
        when(teiidType.getName()).thenReturn(KomodoLexicon.CachedTeiid.NODE_TYPE);
        Descriptor folderType = Mockito.mock(Descriptor.class);
        when(folderType.getName()).thenReturn(KomodoLexicon.Folder.NODE_TYPE);

        CachedTeiid cachedTeiid = Mockito.mock(CachedTeiid.class);
        Folder vdbFolder = mock(Folder.class);
        Mockito.when(cachedTeiid.getName(transaction)).thenReturn("localhost");
        Mockito.when(cachedTeiid.getAbsolutePath()).thenReturn(parentDataPath);
        Mockito.when(cachedTeiid.getTypeIdentifier(transaction)).thenReturn(KomodoType.CACHED_TEIID);
        Mockito.when(cachedTeiid.getPrimaryType(transaction)).thenReturn(teiidType);
        Mockito.when(vdbFolder.getName(transaction)).thenReturn("localhost/" + CachedTeiid.VDBS_FOLDER);
        Mockito.when(vdbFolder.getPrimaryType(transaction)).thenReturn(folderType);
        
        Vdb theVdb = Mockito.mock(Vdb.class);
        Mockito.when(theVdb.getName(transaction)).thenReturn(VDB_NAME);
        Mockito.when(theVdb.getAbsolutePath()).thenReturn(parentDataPath + FORWARD_SLASH + VDB_NAME);
        Mockito.when(theVdb.getTypeIdentifier(transaction)).thenReturn(kType);
        Mockito.when(theVdb.hasChildren(transaction)).thenReturn(true);
        Mockito.when(theVdb.getPropertyNames(transaction)).thenReturn(new String[0]);
        Mockito.when(theVdb.getPropertyDescriptors(transaction)).thenReturn(new PropertyDescriptor[0]);
        Mockito.when(theVdb.getParent(transaction)).thenReturn(vdbFolder);
        Mockito.when(vdbFolder.getParent(transaction)).thenReturn(cachedTeiid);

        RestVdb restVdb = new RestVdb(BASE_URI, theVdb, false, transaction);
        assertEquals(BASE_URI, restVdb.getBaseUri());
        assertEquals(parentDataPath + FORWARD_SLASH + VDB_NAME, restVdb.getDataPath());
        Collection<RestLink> links = restVdb.getLinks();
        assertNotNull(links);
        for (RestLink link :  links) {
            switch (link.getRel()) {
                case SELF:
                    assertEquals(BASE_TEIID_URI + FORWARD_SLASH +
                                             cachedTeiid.getName(transaction) + FORWARD_SLASH +
                                             V1Constants.VDBS_SEGMENT + FORWARD_SLASH +
                                             VDB_NAME,
                                             link.getHref().toString());
                    break;
                case PARENT:
                    assertEquals(BASE_TEIID_URI + FORWARD_SLASH +
                                             cachedTeiid.getName(transaction) + FORWARD_SLASH +
                                             V1Constants.VDBS_SEGMENT,
                                             link.getHref().toString());
                    break;
            }
        }
    }
}
