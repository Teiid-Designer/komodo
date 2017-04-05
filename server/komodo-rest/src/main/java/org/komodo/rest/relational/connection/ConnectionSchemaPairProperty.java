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
package org.komodo.rest.relational.connection;

import java.util.ArrayList;
import java.util.List;

/**
    "property": {
        "keng__properties": {
            "value": {
                "keng__type": "string",
                "keng__required": "false"
            },
            "name": {
                "keng__type": "string",
                "keng__required": "false"
            }
        }
        "keng__type": "string",
        "keng__required": "false",
        "keng__repeatable": "true",
        "keng__limit": "-1"
    }
 */
public class ConnectionSchemaPairProperty extends ConnectionSchemaProperty {

    public static final String PROPERTY_NAME_LABEL = "name";

    public static final String PROPERTY_VALUE_LABEL = "value";

    private List<ConnectionSchemaProperty> properties = new ArrayList<ConnectionSchemaProperty>();

    public ConnectionSchemaPairProperty(String name) {
        super(name, "string", false, true, -1);

        properties.add(new ConnectionSchemaProperty(PROPERTY_NAME_LABEL));
        properties.add(new ConnectionSchemaProperty(PROPERTY_VALUE_LABEL));
    }

    public List<ConnectionSchemaProperty> getProperties() {
        return properties;
    }

    public void setProperties(List<ConnectionSchemaProperty> properties) {
        this.properties = properties;
    }
}
