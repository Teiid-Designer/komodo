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

import java.lang.annotation.Annotation;
import java.lang.reflect.Type;
import java.util.Iterator;
import org.komodo.rest.RestLink;
import org.komodo.rest.json.JsonConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.utils.KLog;
import com.fasterxml.jackson.databind.JavaType;
import io.swagger.converter.ModelConverter;
import io.swagger.converter.ModelConverterContext;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.StringProperty;
import io.swagger.util.Json;

/**
 * @param <T> Class to be converted
 */
public abstract class RestEntityConverter<T> implements ModelConverter, JsonConstants {

    protected static final KLog LOGGER = KLog.getLogger();

    protected abstract Class<T> getEntityClass();

    protected abstract KomodoType getKomodoType();

    protected abstract void addProperties(ModelImpl model, ModelConverterContext context) throws Exception;

    @Override
    public Property resolveProperty(Type type, ModelConverterContext context, Annotation[] annotations, Iterator<ModelConverter> chain) {
        if (chain.hasNext())
            return chain.next().resolveProperty(type, context, annotations, chain);

        return null;
    }

    protected boolean isApplicable(Type type, Class<T> klazz) {
        if (type == null)
            return false;

        JavaType _type = Json.mapper().constructType(type);
        if (_type == null)
            return false;

        Class<?> cls = _type.getRawClass();

        if (! klazz.isAssignableFrom(cls))
            return false;

        return true;
    }

    protected Model defaultAction(Type type, ModelConverterContext context, Iterator<ModelConverter> chain) {
        if (chain.hasNext())
            return chain.next().resolve(type, context, chain);

        return null;
    }

    protected ModelImpl generateRestEntityModel(String name, ModelConverterContext context) throws Exception {
        ModelImpl model = new ModelImpl();
        model.setName(name);

        model.property(ID, requiredProperty(String.class));
        model.property(DATA_PATH, requiredProperty(String.class));

        StringProperty kTypeProperty = new StringProperty();
        kTypeProperty.setRequired(true);
        kTypeProperty.readOnly();
        kTypeProperty._enum(getKomodoType().toString());
        model.property(KTYPE, kTypeProperty);

        model.property(HAS_CHILDREN, requiredProperty(Boolean.class));

        model.property(LINKS, context.resolveProperty(RestLink.class, null));

        context.defineModel(model.getName(), model);

        return model;
    }

    protected Property property(Class<?> typeClass) throws Exception {
        Property property = null;
        if (String.class.equals(typeClass))
            property = new StringProperty();
        else if (Integer.class.equals(typeClass))
            property = new IntegerProperty();
        else if (Boolean.class.equals(typeClass))
            property = new BooleanProperty();
        else
            throw new Exception("Unsupported property type " +  typeClass); //$NON-NLS-1$
        return property;
    }

    protected Property requiredProperty(Class<?> typeClass) throws Exception {
        Property property = property(typeClass);
        property.setRequired(true);
        return property;
    }

    @SuppressWarnings( "nls" )
    @Override
    public Model resolve(Type type, ModelConverterContext context, Iterator<ModelConverter> chain) {
        if (!isApplicable(type, getEntityClass()))
            return defaultAction(type, context, chain);

        ModelImpl model;
        try {
            model = generateRestEntityModel(getEntityClass().getSimpleName(), context);
            addProperties(model, context);

        } catch (Exception ex) {
            LOGGER.error("Exception occurred whilst resolving the model type " + type.toString());
            return null;
        }

        return model;
    }
}
