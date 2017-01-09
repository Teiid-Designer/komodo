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
package org.komodo.rest.swagger;

import org.komodo.rest.RestProperty;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.spi.repository.KomodoType;
import io.swagger.converter.ModelConverterContext;
import io.swagger.models.ModelImpl;

/**
 * Converter to display properties of {@link RestVdb} class in swagger
 */
public class RestVdbConverter extends RestEntityConverter<RestVdb> {

    @Override
    protected Class<RestVdb> getEntityClass() {
        return RestVdb.class;
    }

    @Override
    protected KomodoType getKomodoType() {
        return KomodoType.VDB;
    }

    @Override
    protected void addProperties(ModelImpl model, ModelConverterContext context) throws Exception {
        model.property(RestVdb.NAME_LABEL, requiredProperty(String.class));
        model.property(RestVdb.DESCRIPTION_LABEL, property(String.class));
        model.property(RestVdb.FILE_PATH_LABEL, requiredProperty(String.class));
        model.property(RestVdb.PREVIEW_LABEL, property(Boolean.class));
        model.property(RestVdb.CONNECTION_TYPE_LABEL, requiredProperty(String.class));
        model.property(RestVdb.VERSION_LABEL, property(String.class));

        model.property(PROPERTIES, context.resolveProperty(RestProperty.class, null));
    }
}
