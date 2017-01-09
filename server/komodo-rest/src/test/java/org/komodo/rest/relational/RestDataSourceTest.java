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

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.ws.rs.core.UriBuilder;

import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.folder.Folder;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.repository.DescriptorImpl;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.RestProperty;
import org.komodo.rest.relational.datasource.RestDataSource;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.repository.Repository.UnitOfWorkListener;
import org.mockito.Matchers;
import org.mockito.Mockito;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestDataSourceTest implements V1Constants {

    private static final String NAME = "MyDataSource";

    private static final String DRIVER_NAME = "MySql";

    private static final String JNDI_NAME = "MyDataSourceJndiName";

    private static final String BASE_URI_PREFIX = "http://localhost:8081/v1";

    private static final URI MY_BASE_URI = UriBuilder.fromUri(BASE_URI_PREFIX).build();

    private static final String TEIID_SERVER = "DefaultServer";

    private UnitOfWork transaction;

    private Repository repository;

    private RestDataSource dataSource;

    @Before
    public void init() throws Exception {
        transaction = Mockito.mock(UnitOfWork.class);
        when(transaction.getState()).thenReturn(State.NOT_STARTED);

        UnitOfWork uow = Mockito.mock(UnitOfWork.class);
        when(uow.getState()).thenReturn(State.NOT_STARTED);

        repository = Mockito.mock(Repository.class);
        UnitOfWorkListener listener = Matchers.any();
        when(repository.createTransaction(Matchers.anyString(),
                                                  Matchers.anyString(),
                                                  Matchers.anyBoolean(),
                                                  listener)).thenReturn(uow);

        this.dataSource = new RestDataSource();
        this.dataSource.setId(NAME);
        this.dataSource.setDriverName(DRIVER_NAME);
        this.dataSource.setJdbc(true);
        this.dataSource.setJndiName(JNDI_NAME);
    }

    @Test
    public void shouldBeEqual() {
        final RestDataSource thatDataSource = new RestDataSource();
        thatDataSource.setId(this.dataSource.getId());
        thatDataSource.setDriverName(this.dataSource.getDriverName());
        thatDataSource.setJndiName(this.dataSource.getJndiName());
        thatDataSource.setJdbc(this.dataSource.isJdbc());
        thatDataSource.setLinks( this.dataSource.getLinks() );

        assertThat( this.dataSource, is( thatDataSource ) );
    }

    @Test
    public void shouldBeEqualWhenComparingEmptyDataSources() {
        final RestDataSource empty1 = new RestDataSource();
        final RestDataSource empty2 = new RestDataSource();
        assertThat( empty1, is( empty2 ) );
    }

    @Test
    public void shouldConstructEmptyDataSource() {
        final RestDataSource empty = new RestDataSource();
        assertThat( empty.getId(), is( nullValue() ) );
        assertThat( empty.getDriverName(), is( nullValue() ) );
        assertThat( empty.getJndiName(), is( nullValue() ) );
        assertThat( empty.isJdbc(), is( false ) );
        assertThat( empty.getProperties().isEmpty(), is( true ) );
        assertThat( empty.getLinks().size(), is( 0 ) );
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestDataSource thatDataSource = new RestDataSource();
        thatDataSource.setId(this.dataSource.getId());
        thatDataSource.setDriverName(this.dataSource.getDriverName());
        thatDataSource.setJndiName(this.dataSource.getJndiName());
        thatDataSource.setJdbc(this.dataSource.isJdbc());
        thatDataSource.setProperties( this.dataSource.getProperties() );
        thatDataSource.setLinks( this.dataSource.getLinks() );

        assertThat( this.dataSource.hashCode(), is( thatDataSource.hashCode() ) );
    }

    @Test
    public void shouldNotBeEqualWhenNameIsDifferent() {
        final RestDataSource thatDataSource = new RestDataSource();
        thatDataSource.setId(this.dataSource.getId() + "blah");
        thatDataSource.setDriverName(this.dataSource.getDriverName());
        thatDataSource.setJndiName(this.dataSource.getJndiName());
        thatDataSource.setJdbc(this.dataSource.isJdbc());
        thatDataSource.setProperties( this.dataSource.getProperties() );
        thatDataSource.setLinks( this.dataSource.getLinks() );

        assertThat( this.dataSource.getId(), is( not( thatDataSource.getId() ) ) );
        assertThat( this.dataSource, is( not( thatDataSource ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenPropertiesAreDifferent() {
        final RestDataSource thatDataSource = new RestDataSource();
        thatDataSource.setId(this.dataSource.getId());
        thatDataSource.setDriverName(this.dataSource.getDriverName());
        thatDataSource.setJndiName(this.dataSource.getJndiName());
        thatDataSource.setJdbc(this.dataSource.isJdbc());
        thatDataSource.setLinks( this.dataSource.getLinks() );

        List<RestProperty> props = new ArrayList<RestProperty>();
        props.addAll(this.dataSource.getProperties() );
        props.add(new RestProperty("blah", "blah" ));
        thatDataSource.setProperties( props );

        assertThat( this.dataSource, is( not( thatDataSource ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenJndiNameIsDifferent() {
        final RestDataSource thatDataSource = new RestDataSource();
        thatDataSource.setId(this.dataSource.getId());
        thatDataSource.setId(this.dataSource.getId());
        thatDataSource.setDriverName(this.dataSource.getDriverName());
        thatDataSource.setJndiName(this.dataSource.getJndiName() + "blah");
        thatDataSource.setJdbc(this.dataSource.isJdbc());
        thatDataSource.setProperties( this.dataSource.getProperties() );
        thatDataSource.setLinks( this.dataSource.getLinks() );

        assertThat( this.dataSource.getJndiName(), is( not( thatDataSource.getJndiName() ) ) );
        assertThat( this.dataSource, is( not( thatDataSource ) ) );
    }

    @Test
    public void shouldHaveCorrectLinks() throws Exception {
        String name = "mysql";
        String dataPath = "/teiidCache/" + TEIID_SERVER + FORWARD_SLASH + name;
        KomodoType kType = KomodoType.DATASOURCE;
        boolean hasChildren = false;

        Descriptor cachedTeiidType = new DescriptorImpl(repository, KomodoLexicon.CachedTeiid.NODE_TYPE);
        Descriptor folderType = new DescriptorImpl(repository, KomodoLexicon.Folder.NODE_TYPE);
        CachedTeiid cachedTeiid = mock(CachedTeiid.class);
        Folder dataSourceFolder = mock(Folder.class);
        when(cachedTeiid.getName(transaction)).thenReturn(TEIID_SERVER);
        when(cachedTeiid.getPrimaryType(transaction)).thenReturn(cachedTeiidType);
        when(dataSourceFolder.getName(transaction)).thenReturn(TEIID_SERVER + FORWARD_SLASH + CachedTeiid.DATA_SOURCES_FOLDER);
        when(dataSourceFolder.getPrimaryType(transaction)).thenReturn(folderType);
        
        Datasource dataSource = mock(Datasource.class);
        when(dataSource.getName(transaction)).thenReturn(name);
        when(dataSource.getAbsolutePath()).thenReturn(dataPath);
        when(dataSource.getParent(transaction)).thenReturn(dataSourceFolder);
        when(dataSourceFolder.getParent(transaction)).thenReturn(cachedTeiid);
        when(dataSource.getTypeIdentifier(transaction)).thenReturn(kType);
        when(dataSource.hasChildren(transaction)).thenReturn(hasChildren);
        when(dataSource.getRepository()).thenReturn(repository);
        when(dataSource.getPropertyNames(transaction)).thenReturn(new String[0]);
        when(dataSource.getPropertyDescriptors(transaction)).thenReturn(new PropertyDescriptor[0]);

        RestDataSource restDataSource = new RestDataSource(MY_BASE_URI, dataSource, transaction);

        Collection<RestLink> links = restDataSource.getLinks();
        assertEquals(3, links.size());

        int linkCounter = 0;
        for (RestLink link : links) {
            String href = link.getHref().toString();

            if (LinkType.SELF.equals(link.getRel())) {
                linkCounter++;
                assertEquals(BASE_URI_PREFIX +
                                         FORWARD_SLASH + TEIID_SEGMENT +
                                         FORWARD_SLASH + TEIID_SERVER +
                                         FORWARD_SLASH + CachedTeiid.DATA_SOURCES_FOLDER +
                                         FORWARD_SLASH + name, href);
            } else if (LinkType.PARENT.equals(link.getRel())) {
                linkCounter++;
                assertEquals(BASE_URI_PREFIX +
                                         FORWARD_SLASH + TEIID_SEGMENT +
                                         FORWARD_SLASH + TEIID_SERVER +
                                         FORWARD_SLASH + CachedTeiid.DATA_SOURCES_FOLDER, href);
            } else if (LinkType.CHILDREN.equals(link.getRel())) {
                linkCounter++;
            }
        }

        assertEquals(3, linkCounter);
    }
}
