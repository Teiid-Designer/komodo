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
package org.komodo.rest.relational.dataservice;

import java.util.ArrayList;
import java.util.List;
import javax.ws.rs.core.MediaType;
import org.komodo.core.KomodoLexicon;
import org.komodo.rest.KRestEntity;
import org.komodo.rest.KomodoService;
import org.komodo.spi.repository.KomodoType;

public class DataServiceSchema implements KRestEntity {

    /**
     * Label for the whole data source schema
     */
    public static final String NAME_LABEL = KomodoType.DATASERVICE.name().toLowerCase();

    /**
     * Label for the id
     */
    public static final String ID_LABEL = "keng__id";

    /**
     * Label for the ktype
     */
    public static final String KTYPE_LABEL = "keng__kType";

    /**
     * Label for the description
     */
    public static final String DESCRIPTION_LABEL = KomodoService.protectPrefix(KomodoLexicon.LibraryComponent.DESCRIPTION);

    /**
     * Label for the properties
     */
    public static final String PROPERTIES_LABEL = "keng__properties";

    private String id = KomodoType.DATASERVICE.name().toLowerCase();

    private String kType = KomodoType.DATASERVICE.getType();

    private String description = "A description for the dataservice";

    private List<DataServiceSchemaProperty> properties = new ArrayList<DataServiceSchemaProperty>(4);

    public DataServiceSchema() {
    }

    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getkType() {
        return kType;
    }

    public void setkType(String kType) {
        this.kType = kType;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public List<DataServiceSchemaProperty> getProperties() {
        return properties;
    }

    public void setProperties(List<DataServiceSchemaProperty> properties) {
        this.properties = properties;
    }
}
