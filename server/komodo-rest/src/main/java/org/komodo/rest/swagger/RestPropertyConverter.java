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

import java.lang.reflect.Type;
import java.util.Iterator;
import org.komodo.rest.RestProperty;
import org.komodo.spi.repository.KomodoType;
import io.swagger.converter.ModelConverter;
import io.swagger.converter.ModelConverterContext;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;

/**
 * Converter to display properties of {@link RestProperty} class in swagger
 */
public class RestPropertyConverter extends RestEntityConverter<RestProperty> {

    @Override
    protected Class<RestProperty> getEntityClass() {
        return RestProperty.class;
    }

    @Override
    protected KomodoType getKomodoType() {
        return KomodoType.UNKNOWN; // Not required
    }

    @Override
    protected void addProperties(ModelImpl model, ModelConverterContext context) throws Exception {
        model.property(RestProperty.NAME_LABEL, requiredProperty(String.class));
        model.property(RestProperty.VALUE_LABEL, property(String.class));
    }

    @SuppressWarnings( "nls" )
    @Override
    public Model resolve(Type type, ModelConverterContext context, Iterator<ModelConverter> chain) {

        if (!isApplicable(type, getEntityClass()))
            return defaultAction(type, context, chain);

        ModelImpl model;
        try {
            model = new ModelImpl();
            model.setName(getEntityClass().getSimpleName());
            addProperties(model, context);

        } catch (Exception ex) {
            LOGGER.error("Exception occurred whilst resolving the model type " + type.toString());
            return null;
        }

        return model;
    }
}
