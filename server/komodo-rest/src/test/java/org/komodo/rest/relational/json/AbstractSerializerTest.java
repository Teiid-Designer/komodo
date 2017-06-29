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
package org.komodo.rest.relational.json;

import java.net.URI;
import javax.ws.rs.core.UriBuilder;
import org.junit.Before;
import org.komodo.rest.json.JsonConstants;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.repository.Repository.UnitOfWorkListener;
import org.mockito.Matchers;
import org.mockito.Mock;
import org.mockito.Mockito;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls" })
public abstract class AbstractSerializerTest implements JsonConstants {

    protected static final String BASE_URI_PREFIX = "http://localhost:8081/v1";

    protected static final URI MY_BASE_URI = UriBuilder.fromUri(BASE_URI_PREFIX).build();

    protected static final String WORKSPACE_DATA_PATH = "/workspace";
    protected static final String VDB_NAME = "vdb1";
    protected static final String VDB_DATA_PATH = "/workspace/vdbs/vdb1";
    protected static final String DATASERVICE_NAME = "dataservice1";
    protected static final String DATASERVICE_DATA_PATH = "/workspace/dataservices/dataservice1";
    protected static final String SEARCH = "/workspace/search?";

    @Mock
    protected UnitOfWork transaction;

    protected Repository repository;

    protected static String q(String value) {
        return SPEECH_MARK + value + SPEECH_MARK;
    }

    protected static String colon() {
        return COLON + SPACE;
    }

    protected static String tab(int freq) {
        StringBuffer buf = new StringBuffer();
        for (int i = 0; i < freq; ++i)
            buf.append(SPACE + SPACE);

        return buf.toString();
    }

    public AbstractSerializerTest() {
        super();
    }

    @Before
    public void basicInit() throws KException {
        transaction = Mockito.mock(UnitOfWork.class);
        Mockito.when(transaction.getState()).thenReturn(State.NOT_STARTED);

        UnitOfWork uow = Mockito.mock(UnitOfWork.class);
        Mockito.when(uow.getState()).thenReturn(State.NOT_STARTED);

        repository = Mockito.mock(Repository.class);
        UnitOfWorkListener listener = Matchers.any();
        Mockito.when(repository.createTransaction(Matchers.anyString(),
                                                  Matchers.anyString(),
                                                  Matchers.anyBoolean(),
                                                  listener)).thenReturn(uow);
    }

    protected <T extends KomodoObject> T mockObject(Class<T> mockClass, String name, String dataPath, KomodoType kType, boolean hasChildren, String descriptorName) throws KException {
        T kObject = Mockito.mock(mockClass);
        Mockito.when(kObject.getName(transaction)).thenReturn(name);
        Mockito.when(kObject.getAbsolutePath()).thenReturn(dataPath);
        Mockito.when(kObject.getTypeIdentifier(transaction)).thenReturn(kType);
        Mockito.when(kObject.hasChildren(transaction)).thenReturn(hasChildren);
        Mockito.when(kObject.getRepository()).thenReturn(repository);

        if (descriptorName != null) {
            Descriptor primaryDescriptor = Mockito.mock(Descriptor.class);
            Mockito.when(primaryDescriptor.getName()).thenReturn(descriptorName.toString());
            Mockito.when(kObject.getPrimaryType(transaction)).thenReturn(primaryDescriptor);
        }

        return kObject;
    }

    protected <T extends KomodoObject> T mockObject(Class<T> mockClass, String name, String dataPath, KomodoType kType, boolean hasChildren) throws KException {
        return mockObject(mockClass, name, dataPath, kType, hasChildren, null);
    }
}
