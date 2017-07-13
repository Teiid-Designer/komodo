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
import org.komodo.rest.relational.connection.RestTemplate;
import org.komodo.rest.relational.json.BasicEntitySerializer;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@link RestTemplate}s.
 */
public final class TemplateSerializer extends BasicEntitySerializer<RestTemplate> {

    @Override
    protected RestTemplate createEntity() {
        return new RestTemplate();
    }

    @Override
    public String readExtension(String name, RestTemplate entity, JsonReader in) {
        if (RestTemplate.ENTRIES_LABEL.equals(name)) {
            String[] entries = BUILDER.fromJson(in, String[].class);
            entity.setEntries(entries);
            return RestTemplate.ENTRIES_LABEL;
        }

        return null;
    }

    @Override
    public void writeExtensions(final JsonWriter out, final RestTemplate entity) throws IOException {
        if (entity.getEntries().size() > 0) {
            out.name(RestTemplate.ENTRIES_LABEL);
            BUILDER.toJson(entity.getEntries().toArray(new String[0]), String[].class, out);
        }
    }
}
