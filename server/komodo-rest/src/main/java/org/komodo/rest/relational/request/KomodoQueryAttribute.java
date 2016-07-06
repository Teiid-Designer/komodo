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
package org.komodo.rest.relational.request;

import javax.ws.rs.core.MediaType;
import org.codehaus.jackson.annotate.JsonIgnore;
import org.codehaus.jackson.annotate.JsonProperty;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.codehaus.jackson.map.annotate.JsonSerialize.Inclusion;
import org.komodo.rest.KRestEntity;
import org.komodo.spi.query.QueryService;


/**
 * Object to be serialised by GSON that encapsulates an artifact path object
 */
@JsonSerialize(include=Inclusion.NON_NULL)
public class KomodoQueryAttribute implements KRestEntity {

    /**
     * Label for the query
     */
    public static final String QUERY_LABEL = "query"; //$NON-NLS-1$

    /**
     * Label for the target database
     */
    public static final String TARGET_LABEL = "target";

    /**
     * Label for the limit of results
     */
    public static final String LIMIT_LABEL = "limit";

    /**
     * Label for the offset of results
     */
    public static final String OFFSET_LABEL = "offset";

    @JsonProperty(QUERY_LABEL)
    private String query;

    @JsonProperty(TARGET_LABEL)
    private String target;

    @JsonProperty(LIMIT_LABEL)
    private int limit = QueryService.NO_LIMIT;

    @JsonProperty(OFFSET_LABEL)
    private int offset = QueryService.NO_OFFSET;

    /**
     * Default constructor for deserialization
     */
    public KomodoQueryAttribute() {
        // do nothing
    }

    @Override
    @JsonIgnore
    public Object getXml() {
        throw new UnsupportedOperationException();
    }

    @Override
    @JsonIgnore
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType);
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
     * @return the target
     */
    public String getTarget() {
        return target;
    }

    /**
     * @param target the target to set
     */
    public void setTarget(String target) {
        this.target = target;
    }

    /**
     * @return the result limit
     */
    public int getLimit() {
        return limit;
    }

    /**
     * @param limit the result limit
     */
    public void setLimit(int limit) {
        this.limit = limit;
    }

    /**
     * @return the result offset
     */
    public int getOffset() {
        return offset;
    }

    /**
     * @param offset the minimum record of the result set to start at
     */
    public void setOffset(int offset) {
        this.offset = offset;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + limit;
        result = prime * result + offset;
        result = prime * result + ((query == null) ? 0 : query.hashCode());
        result = prime * result + ((target == null) ? 0 : target.hashCode());
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
        KomodoQueryAttribute other = (KomodoQueryAttribute)obj;
        if (limit != other.limit)
            return false;
        if (offset != other.offset)
            return false;
        if (query == null) {
            if (other.query != null)
                return false;
        } else if (!query.equals(other.query))
            return false;
        if (target == null) {
            if (other.target != null)
                return false;
        } else if (!target.equals(other.target))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "KomodoQueryAttribute [query=" + query + ", target=" + target + ", limit=" + limit + ", offset=" + offset + "]";
    }
}
