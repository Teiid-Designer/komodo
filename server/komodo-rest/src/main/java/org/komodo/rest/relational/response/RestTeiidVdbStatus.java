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
package org.komodo.rest.relational.response;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidVdb;

/**
 * A Teiid Vdb Status object that can be used by GSON to build a JSON document representation.
 */
public class RestTeiidVdbStatus extends RestBasicEntity {

    /**
     * Label for the vdbs collection
     */
    public static final String VDBS_LABEL = "vdbs";

    private List<RestTeiidVdbStatusVdb> vdbProps = new ArrayList<>();

    /**
     * Constructor for use when deserializing
     */
    public RestTeiidVdbStatus() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri
     * @param vdb the teiid object
     * @param uow the transaction
     *
     * @throws KException if error occurs
     */
    public RestTeiidVdbStatus(URI baseUri, Teiid teiid, UnitOfWork uow) throws KException {
        super(baseUri);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().teiidVdbStatusUri()));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().teiidStatusUri()));

        synchronized (TeiidInstance.TEIID_INSTANCE_LOCK) {
            TeiidInstance teiidInstance = teiid.getTeiidInstance(uow);

            if (teiidInstance == null)
                return;

            try {
                teiidInstance.connect();

                Collection<TeiidVdb> vdbs = teiidInstance.getVdbs();
                for (TeiidVdb vdb :  vdbs) {
                    RestTeiidVdbStatusVdb props = new RestTeiidVdbStatusVdb(vdb);
                    vdbProps .add(props);
                }

            } catch (Exception ex) {
                throw new KException(ex);
            }
        }
    }

    public List<RestTeiidVdbStatusVdb> getVdbProperties() {
        return vdbProps;
    }

    public void setVdbProperties(List<RestTeiidVdbStatusVdb> props) {
        if (props == null)
            props = new ArrayList<>();

        this.vdbProps = props;
    }
}
