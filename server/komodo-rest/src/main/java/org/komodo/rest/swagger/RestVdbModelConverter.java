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

import org.komodo.relational.model.Model;
import org.komodo.rest.RestProperty;
import org.komodo.rest.relational.response.RestVdbModel;
import org.komodo.spi.repository.KomodoType;
import io.swagger.converter.ModelConverterContext;
import io.swagger.models.ModelImpl;
import io.swagger.models.properties.StringProperty;

/**
 * Converter to display properties of {@link RestVdbModel} class in swagger
 */
public class RestVdbModelConverter extends RestEntityConverter<RestVdbModel> {

    @Override
    protected Class<RestVdbModel> getEntityClass() {
        return RestVdbModel.class;
    }

    @Override
    protected KomodoType getKomodoType() {
        return KomodoType.MODEL;
    }

    @Override
    protected void addProperties(ModelImpl model, ModelConverterContext context) throws Exception {
        model.property(RestVdbModel.DESCRIPTION_LABEL, property(String.class));

        StringProperty modelTypeProperty = (StringProperty) requiredProperty(String.class);
        modelTypeProperty._enum(Model.Type.PHYSICAL.toString());
        modelTypeProperty._enum(Model.Type.VIRTUAL.toString());
        model.property(RestVdbModel.MODEL_TYPE_LABEL, modelTypeProperty);

        model.property(RestVdbModel.VISIBLE_LABEL, property(Boolean.class));
        model.property(RestVdbModel.METADATA_TYPE_LABEL, requiredProperty(String.class));
        model.property(DDL_ATTRIBUTE, property(String.class));

        model.property(PROPERTIES, context.resolveProperty(RestProperty.class, null));
    }
}
