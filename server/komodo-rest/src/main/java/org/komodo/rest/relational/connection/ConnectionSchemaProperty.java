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

import org.komodo.rest.KomodoService;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

public class ConnectionSchemaProperty {

    /**
     * Label used to describe jndi name
     */
    public static final String JNDI_NAME_LABEL = KomodoService.protectPrefix(DataVirtLexicon.Connection.JNDI_NAME);

    /**
     * Label used to describe driver name
     */
    public static final String DRIVER_NAME_LABEL = KomodoService.protectPrefix(DataVirtLexicon.Connection.DRIVER_NAME);

    public static final String TYPE_LABEL = "keng__type";

    public static final String REQUIRED_LABEL = "keng__required";

    public static final String REPEATABLE_LABEL = "keng__repeatable";

    public static final String LIMIT_LABEL = "keng__limit";

    public static final String PROPERTIES_NAME_LABEL = "property";

    private String name;

    private String type = "string";

    private boolean required = true;

    private boolean repeatable = false;

    private int limit = 1;

    public ConnectionSchemaProperty(String name, String type, boolean required, boolean repeatable, int limit) {
        setName(name);
        setType(type);
        setRequired(required);
        setRepeatable(repeatable);
        setLimit(limit);
    }

    public ConnectionSchemaProperty(String name) {
        setName(name);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public boolean isRequired() {
        return required;
    }

    public void setRequired(boolean required) {
        this.required = required;
    }

    public boolean isRepeatable() {
        return repeatable;
    }

    public void setRepeatable(boolean repeatable) {
        this.repeatable = repeatable;
    }

    public int getLimit() {
        return limit;
    }

    public void setLimit(int limit) {
        this.limit = limit;
    }
}