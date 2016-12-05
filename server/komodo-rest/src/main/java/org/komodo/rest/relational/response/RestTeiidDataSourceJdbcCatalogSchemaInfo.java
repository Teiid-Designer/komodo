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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import javax.ws.rs.core.MediaType;
import org.komodo.rest.KRestEntity;
import org.komodo.spi.KException;

/**
 * A DataSource JDBC Catalog and Schema info object that can be used by GSON to build a JSON document representation.
 */
public final class RestTeiidDataSourceJdbcCatalogSchemaInfo implements KRestEntity {

    /**
     * Type identifier for Catalogs
     */
    public static final String TYPE_CATALOG = "Catalog"; //$NON-NLS-1$

    /**
     * Type identifier for Schema
     */
    public static final String TYPE_SCHEMA = "Schema"; //$NON-NLS-1$
    
    /**
     * Label used to indicate the name of the item
     */
    public static final String TEIID_JDBC_ITEM_NAME_LABEL = "itemName"; //$NON-NLS-1$

    /**
     * Label used to indicate the type of the item (Catalog or Schema)
     */
    public static final String TEIID_JDBC_ITEM_TYPE_LABEL = "itemType"; //$NON-NLS-1$

    /**
     * Label used to indicate child schema names (only used if itemType is a Catalog)
     */
    public static final String TEIID_JDBC_CATALOG_SCHEMA_NAMES_LABEL = "catalogSchema"; //$NON-NLS-1$

    private String name;
    private String type;
    private List<String> schemaNames;
    
    /**
     * Constructor for use when deserializing
     */
    public RestTeiidDataSourceJdbcCatalogSchemaInfo() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param itemName the name of the item
     * @param itemType the type of item (schema or catalog)
     * @param catalogSchemaNames the list of schema (only used for for catalogs supporting schema)
     *
     * @throws KException if error occurs
     */
    public RestTeiidDataSourceJdbcCatalogSchemaInfo(String itemName, String itemType, List<String> catalogSchemaNames) throws KException {
        super();

        setItemName(itemName);
        setItemType(itemType);

        setCatalogSchemaNames(catalogSchemaNames);
    }
    
    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    /**
     * @return the item name
     */
    public String getItemName() {
        return name;
    }

    /**
     * @param itemName the item name
     */
    public void setItemName(String itemName) {
        this.name = itemName;
    }
    
    /**
     * @return the item type
     */
    public String getItemType() {
        return type;
    }

    /**
     * @param itemType the item type
     */
    public void setItemType(String itemType) {
        this.type = itemType;
    }
    
    /**
     * @return the list of schema names for a catalog item
     */
    public String[] getCatalogSchemaNames() {
        return schemaNames != null ? (String[]) schemaNames.toArray(new String[0]) : EMPTY_ARRAY;
    }

    /**
     * @param schemaNames the schema names for a catalog
     */
    public void setCatalogSchemaNames(Collection<String> schemaNames) {
        if (schemaNames == null || schemaNames.size() == 0)
            this.schemaNames = Collections.emptyList();

        this.schemaNames = new ArrayList<String>();
        for (Object value: schemaNames) {
            this.schemaNames.add(value.toString());
        }
    }

}
