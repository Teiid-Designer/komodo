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
package org.komodo.rest.relational;

import javax.ws.rs.core.MediaType;
import org.komodo.rest.KRestEntity;

public class ImportExportStatus implements KRestEntity {

    /**
     * Label for name
     */
    public static final String NAME_LABEL = "Name"; //$NON-NLS-1$

    /**
     * Label for type
     */
    public static final String TYPE_LABEL = "Type"; //$NON-NLS-1$

    /**
     * Label for downloadable
     */
    public static final String DOWNLOADABLE_LABEL = "Downloadable"; //$NON-NLS-1$

    /**
     * Label for the content
     */
    public static final String CONTENT_LABEL = "Content"; //$NON-NLS-1$

    /**
     * Label for the success flag
     */
    public static final String SUCCESS_LABEL = "Success"; //$NON-NLS-1$

    private String name;

    private String type;

    private boolean downloadable;

    private String content;

    private boolean success;

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    /**
     * @return has downloadable
     */
    public boolean hasDownloadable() {
        return downloadable;
    }

    /**
     * @param downloadable
     */
    public void setDownloadable(boolean downloadable) {
        this.downloadable = downloadable;
    }

    /**
     * @return content if downloadable or null
     */
    public String getContent() {
        return content;
    }

    /**
     * @param content
     */
    public void setContent(String content) {
        this.content = content;
    }

    /**
     * @return success
     */
    public boolean isSuccess() {
        return success;
    }

    /**
     * @param success
     */
    public void setSuccess(boolean success) {
        this.success = success;
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
}
