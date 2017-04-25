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
package org.komodo.rest.relational.json;

import static org.komodo.rest.Messages.Error.UNEXPECTED_JSON_TOKEN;
import java.io.IOException;
import org.komodo.rest.Messages;
import org.komodo.rest.relational.response.ImportExportStatus;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

/**
 * A GSON serializer/deserializer for {@status ImportExportStatus}s.
 */
public final class ImportExportStatusSerializer extends TypeAdapter< ImportExportStatus > {

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#read(com.google.gson.stream.JsonReader)
     */
    @Override
    public ImportExportStatus read( final JsonReader in ) throws IOException {
        final ImportExportStatus status = new ImportExportStatus();
        in.beginObject();

        while ( in.hasNext() ) {
            final String name = in.nextName();

            switch ( name ) {
                case ImportExportStatus.NAME_LABEL:
                    status.setName(in.nextString());
                    break;
                case ImportExportStatus.TYPE_LABEL:
                    status.setType(in.nextString());
                    break;
                case ImportExportStatus.SUCCESS_LABEL:
                    status.setSuccess(in.nextBoolean());
                    break;
                case ImportExportStatus.DOWNLOADABLE_LABEL:
                    status.setDownloadable(in.nextBoolean());
                    break;
                case ImportExportStatus.CONTENT_LABEL:
                    status.setContent(in.nextString());
                    break;
                case ImportExportStatus.MESSAGE_LABEL:
                    status.setMessage(in.nextString());
                    break;
                case ImportExportStatus.DOWNLOADABLE_SIZE_LABEL:
                    status.setDownloadableSize(in.nextLong());
                    break;
                default:
                    throw new IOException( Messages.getString( UNEXPECTED_JSON_TOKEN, name ) );
            }
        }

        in.endObject();

        return status;
    }

    /**
     * {@inheritDoc}
     *
     * @see com.google.gson.TypeAdapter#write(com.google.gson.stream.JsonWriter, java.lang.Object)
     */
    @Override
    public void write( final JsonWriter out,
                       final ImportExportStatus value ) throws IOException {

        out.beginObject();

        out.name(ImportExportStatus.NAME_LABEL);
        out.value(value.getName());

        out.name(ImportExportStatus.TYPE_LABEL);
        out.value(value.getType());

        out.name(ImportExportStatus.SUCCESS_LABEL);
        out.value(value.isSuccess());

        out.name(ImportExportStatus.DOWNLOADABLE_LABEL);
        out.value(value.hasDownloadable());

        out.name(ImportExportStatus.CONTENT_LABEL);
        out.value(value.getContent());

        out.name(ImportExportStatus.MESSAGE_LABEL);
        out.value(value.getMessage());

        out.name(ImportExportStatus.DOWNLOADABLE_SIZE_LABEL);
        out.value(value.getDownloadableSize());

        out.endObject();
    }

}
