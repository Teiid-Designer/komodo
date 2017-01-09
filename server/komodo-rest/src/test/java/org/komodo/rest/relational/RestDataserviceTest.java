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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.when;
import java.net.URI;
import javax.ws.rs.core.UriBuilder;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.mockito.Mockito;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class RestDataserviceTest {

    private static final URI BASE_URI = UriBuilder.fromUri("http://localhost:8081/v1/").build();
    private static final String WORKSPACE_DATA_PATH = "/workspace";
    private static final String DATASERVICE_NAME = "MyDataservice";
    private static final String DATASERVICE_DATA_PATH = "/workspace/dataservices/dataservice1";
    private static final KomodoType kType = KomodoType.DATASERVICE;
    private static final String DESCRIPTION = "my description";
    private static final String SERVICE_VDB_NAME = "serviceVdbName";
    private static final String SERVICE_VDB_VERSION = "1";
    private static final String SERVICE_VIEW_MODEL = "serviceViewModel";
    private static final String SERVICE_VIEW = "serviceView";

    private RestDataservice dataservice;

    private RestDataservice copy() {
        final RestDataservice copy = new RestDataservice();

        copy.setBaseUri(dataservice.getBaseUri());
        copy.setId(dataservice.getId());
        copy.setDescription(dataservice.getDescription());
        copy.setDataPath(dataservice.getDataPath());
        copy.setkType(dataservice.getkType());
        copy.setHasChildren(dataservice.hasChildren());
        copy.setLinks(this.dataservice.getLinks());
        copy.setProperties(this.dataservice.getProperties());
        copy.setServiceVdbName(this.dataservice.getServiceVdbName());
        copy.setServiceVdbVersion(this.dataservice.getServiceVdbVersion());
        copy.setServiceViewModel(this.dataservice.getServiceViewModel());
        copy.setServiceViewName(this.dataservice.getServiceViewName());
        copy.setDriverTotal(this.dataservice.getDriverTotal());
        copy.setConnectionTotal(this.dataservice.getConnectionTotal());

        return copy;
    }

    @Before
    public void init() throws Exception {
        UnitOfWork transaction = Mockito.mock(UnitOfWork.class);

        KomodoObject workspace = Mockito.mock(KomodoObject.class);
        Mockito.when(workspace.getAbsolutePath()).thenReturn(WORKSPACE_DATA_PATH);

        Descriptor dataserviceType = Mockito.mock(Descriptor.class);
        when(dataserviceType.getName()).thenReturn(DataVirtLexicon.DataService.NODE_TYPE);

        Vdb serviceVdb = Mockito.mock(Vdb.class);
        Mockito.when(serviceVdb.getName(transaction)).thenReturn("ServiceVdb");
        Mockito.when(serviceVdb.getVersion(transaction)).thenReturn(1);

        Dataservice theDataservice = Mockito.mock(Dataservice.class);
        Mockito.when(theDataservice.getPrimaryType(transaction)).thenReturn(dataserviceType);
        Mockito.when(theDataservice.getName(transaction)).thenReturn(DATASERVICE_NAME);
        Mockito.when(theDataservice.getAbsolutePath()).thenReturn(DATASERVICE_DATA_PATH);
        Mockito.when(theDataservice.getTypeIdentifier(transaction)).thenReturn(kType);
        Mockito.when(theDataservice.hasChildren(transaction)).thenReturn(true);
        Mockito.when(theDataservice.getPropertyNames(transaction)).thenReturn(new String[0]);
        Mockito.when(theDataservice.getPropertyDescriptors(transaction)).thenReturn(new PropertyDescriptor[0]);
        Mockito.when(theDataservice.getParent(transaction)).thenReturn(workspace);
        Mockito.when(theDataservice.getServiceVdb(transaction)).thenReturn(serviceVdb);
        Mockito.when(theDataservice.getDrivers(transaction)).thenReturn(new Driver[0]);
        Mockito.when(theDataservice.getConnections(transaction)).thenReturn(new Datasource[0]);

        this.dataservice = new RestDataservice(BASE_URI, theDataservice, false, transaction);
        this.dataservice.setId(DATASERVICE_NAME);
        this.dataservice.setDescription(DESCRIPTION);
        this.dataservice.setServiceVdbName(SERVICE_VDB_NAME);
        this.dataservice.setServiceVdbVersion(SERVICE_VDB_VERSION);
        this.dataservice.setServiceViewModel(SERVICE_VIEW_MODEL);
        this.dataservice.setServiceViewName(SERVICE_VIEW);
    }

    @Test
    public void shouldHaveBaseUri() {
        assertEquals(BASE_URI, this.dataservice.getBaseUri());
    }

    @Test
    public void shouldBeEqual() {
        final RestDataservice thatDataservice = copy();
        assertEquals(this.dataservice, thatDataservice);
    }

    @Test
    public void shouldBeEqualWhenComparingEmptyDataservices() {
        final RestDataservice empty1 = new RestDataservice();
        final RestDataservice empty2 = new RestDataservice();
        assertEquals(empty1, empty2);
    }

    @Test
    public void shouldConstructEmptyDataservice() {
        final RestDataservice empty = new RestDataservice();
        assertNull(empty.getBaseUri());
        assertNull(empty.getId());
        assertNull(empty.getDescription());
        assertEquals(empty.getProperties().isEmpty(), true);
        assertEquals(empty.getLinks().size(), 0);
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestDataservice thatDataservice = copy();
        assertEquals(this.dataservice.hashCode(), thatDataservice.hashCode());
    }

    @Test
    public void shouldNotBeEqualWhenNameIsDifferent() {
        final RestDataservice thatDataservice = copy();
        thatDataservice.setId(this.dataservice.getId() + "blah");
        assertNotEquals(this.dataservice, thatDataservice);
    }

    @Test
    public void shouldSetName() {
        final String newName = "blah";
        this.dataservice.setId(newName);
        assertEquals(this.dataservice.getId(), newName);
    }

    @Test
    public void shouldSetDescription() {
        final String newDescription = "blah";
        this.dataservice.setDescription(newDescription);
        assertEquals(this.dataservice.getDescription(), newDescription);
    }

    @Test
    public void shouldSetServiceVdbName() {
        final String newServiceVdb = "blah";
        this.dataservice.setServiceVdbName(newServiceVdb);
        assertEquals(this.dataservice.getServiceVdbName(), newServiceVdb);
    }

    @Test
    public void shouldSetServiceVdbVersion() {
        final String newServiceVdbVersion = "2";
        this.dataservice.setServiceVdbVersion(newServiceVdbVersion);
        assertEquals(this.dataservice.getServiceVdbVersion(), newServiceVdbVersion);
    }

    @Test
    public void shouldSetServiceViewModel() {
        final String newServiceViewModel = "blah";
        this.dataservice.setServiceViewModel(newServiceViewModel);
        assertEquals(this.dataservice.getServiceViewModel(), newServiceViewModel);
    }

    @Test
    public void shouldSetServiceView() {
        final String newServiceView = "blah";
        this.dataservice.setServiceViewName(newServiceView);
        assertEquals(this.dataservice.getServiceViewName(), newServiceView);
    }


}
