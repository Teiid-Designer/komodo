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

import org.komodo.spi.storage.StorageConnector.Descriptor;

public class RestStorageTypeDescriptor {

    public static final String NAME_LABEL = "name";

    public static final String DESCRIPTION_LABEL = "description";

    public static final String REQUIRED_LABEL = "required";

    public static final String ENCODED_LABEL = "encoded";

    private String name;

    private String description;

    private boolean required;

    private boolean encoded;

    public RestStorageTypeDescriptor() {
        super();
    }

    public RestStorageTypeDescriptor(Descriptor descriptor) {
        this.name = descriptor.getName();
        this.description = descriptor.getDescription();
        this.required = descriptor.isRequired();
        this.encoded = descriptor.isEncoded();
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public boolean isRequired() {
        return required;
    }

    public void setRequired(boolean required) {
        this.required = required;
    }

    public boolean isEncoded() {
        return encoded;
    }

    public void setEncoded(boolean encoded) {
        this.encoded = encoded;
    }
}