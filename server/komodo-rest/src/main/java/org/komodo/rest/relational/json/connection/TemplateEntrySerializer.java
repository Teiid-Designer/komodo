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
package org.komodo.rest.relational.json.connection;

import static org.komodo.rest.relational.json.KomodoJsonMarshaller.BUILDER;
import java.io.IOException;
import java.lang.reflect.Type;
import java.util.Map;
import java.util.Properties;
import org.komodo.rest.relational.connection.RestTemplateEntry;
import org.komodo.rest.relational.json.BasicEntitySerializer;
import org.komodo.utils.StringUtils;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestTemplateEntry}s.
 */
public final class TemplateEntrySerializer extends BasicEntitySerializer<RestTemplateEntry> {

    private static final Type STRING_MAP_TYPE = new TypeToken< Map< String, String > >() {/* nothing to do */}.getType();

    @Override
    protected boolean isComplete(final RestTemplateEntry connection) {
        return super.isComplete(connection) && (!StringUtils.isBlank(connection.getId()));
    }

    @Override
    protected RestTemplateEntry createEntity() {
        return new RestTemplateEntry();
    }

    @Override
    protected String readExtension(String name, RestTemplateEntry entity, JsonReader in) {
        if (RestTemplateEntry.CUSTOM_PROPERTIES_LABEL.equals(name)) {
            Properties customProperties = new Properties();
            Map<String, String> properties = BUILDER.fromJson(in, Map.class);
            for (Map.Entry<String, String> property : properties.entrySet()) {
                customProperties.setProperty(property.getKey(), property.getValue());
            }
            entity.setCustomProperties(customProperties);
            return RestTemplateEntry.CUSTOM_PROPERTIES_LABEL;
        }

        return null;
    }

    @Override
    protected void writeExtensions(JsonWriter out, RestTemplateEntry entity) throws IOException {
        if (! entity.getCustomProperties().isEmpty()) {
            out.name(RestTemplateEntry.CUSTOM_PROPERTIES_LABEL);
            BUILDER.toJson(entity.getCustomProperties(), STRING_MAP_TYPE, out);
        }
    }
}
