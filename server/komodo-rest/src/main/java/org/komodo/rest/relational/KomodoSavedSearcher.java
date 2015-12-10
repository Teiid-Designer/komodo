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
import org.komodo.utils.ArgCheck;


/**
 * Object to be serialised by GSON that encapsulates a search object
 */
public class KomodoSavedSearcher implements KRestEntity {

    /**
     * Label for the name
     */
    public static final String NAME_LABEL = "name"; //$NON-NLS-1$

    /**
     * Label for the query
     */
    public static final String QUERY_LABEL = "query"; //$NON-NLS-1$

    private String name;

    private String query;

    /**
     * Default constructor for deserialization
     */
    public KomodoSavedSearcher() {
        // do nothing
    }

    /**
     * @param name the name of this search
     *
     */
    public KomodoSavedSearcher(String name) {
        ArgCheck.isNotNull(name);
        this.name = name;
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
     * @return the query
     */
    public String getQuery() {
        return this.query;
    }

    /**
     * @param query the query to set
     */
    public void setQuery(String query) {
        this.query = query;
    }

    /**
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    /**
     * @param name the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.query == null) ? 0 : this.query.hashCode());
        result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
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
        KomodoSavedSearcher other = (KomodoSavedSearcher)obj;
        if (this.query == null) {
            if (other.query != null)
                return false;
        } else
            if (!this.query.equals(other.query))
                return false;
        if (this.name == null) {
            if (other.name != null)
                return false;
        } else
            if (!this.name.equals(other.name))
                return false;
        return true;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "KomodoSearchObject [name=" + this.name + ", query=" + this.query + "]";
    }
}
