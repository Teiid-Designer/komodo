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

import java.util.LinkedHashMap;
import java.util.Map;
import javax.ws.rs.core.MediaType;
import org.komodo.rest.KRestEntity;
import org.komodo.utils.ArgCheck;


/**
 * Object to be serialised by GSON that provides key/message pairs
 */
public class KomodoStatusObject implements KRestEntity {

    /**
     * Label for the title
     */
    public static final String TITLE_LABEL = "Title"; //$NON-NLS-1$

    /**
     * Label for the information
     */
    public static final String INFO_LABEL = "Information"; //$NON-NLS-1$

    private String title;

    private Map<String, String> attributes = new LinkedHashMap<>();

    /**
     * Default constructor for deserialization
     */
    public KomodoStatusObject() {
        // do nothing
    }

    /**
     * @param title the subject of this status object
     *
     */
    public KomodoStatusObject(String title) {
        ArgCheck.isNotNull(title);
        this.title = title;
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
    }

    @Override
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    /**
     * @return the attributes
     */
    public Map<String, String> getAttributes() {
        return this.attributes;
    }

    /**
     * Add a message pair with a prefixed subject
     *
     * @param subject the subject of the message
     * @param message the message
     */
    public void addAttribute(String subject, String message) {
        attributes.put(subject, message);
    }

    /**
     * @param attributes the attributes to set
     */
    public void setAttributes(Map<String, String> attributes) {
        this.attributes = attributes;
    }

    /**
     * @return the title
     */
    public String getTitle() {
        return this.title;
    }

    /**
     * @param title the title
     */
    public void setTitle(String title) {
        this.title = title;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.attributes == null) ? 0 : this.attributes.hashCode());
        result = prime * result + ((this.title == null) ? 0 : this.title.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        KomodoStatusObject other = (KomodoStatusObject)obj;
        if (this.attributes == null) {
            if (other.attributes != null)
                return false;
        } else
            if (!this.attributes.equals(other.attributes))
                return false;
        if (this.title == null) {
            if (other.title != null)
                return false;
        } else
            if (!this.title.equals(other.title))
                return false;
        return true;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "KomodoStatusObject [title=" + this.title + ", attributes=" + this.attributes + "]";
    }
}
