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
package org.komodo.rest;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoProperties;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;

/**
 * Indicates the objects has a JSON representation.
 */
public class RestBasicEntity implements KRestEntity {

    /**
     * A {@link RestBasicEntity} that indicates the resource was not found.
     */
    public static class ResourceNotFound extends RestBasicEntity {

        private final String operationName;
        private final String resourceName;

        /**
         * @param resourceName
         *        the name of the resource that was not found (cannot be empty)
         * @param operationName
         *        the operation that was executed (cannot be empty)
         */
        public ResourceNotFound(final String resourceName, final String operationName) {
            super();
            ArgCheck.isNotEmpty(resourceName, "resourceName"); //$NON-NLS-1$
            ArgCheck.isNotEmpty(operationName, "operationName"); //$NON-NLS-1$

            this.resourceName = resourceName;
            this.operationName = operationName;
        }

        /**
         * @return the operation name (never empty)
         */
        public String getOperationName() {
            return this.operationName;
        }

        /**
         * @return the resource name (never empty)
         */
        public String getResourceName() {
            return this.resourceName;
        }

    }

    /**
     * Indicates no content is being returned.
     */
    public static final RestBasicEntity NO_CONTENT = new RestBasicEntity() {
        // nothing to do
    };

    private transient KomodoRestUriBuilder uriBuilder;

    protected Map<String, Object> tuples = new LinkedHashMap<>();

    protected List<RestProperty> properties = new ArrayList<>();

    protected Map<LinkType, RestLink> links = RestLink.NO_LINKS;

    // Transient to ensure its never serialized by Gson
    private transient String xml;

    /**
     * Used for NO_CONTENT and ResourceNotFound
     */
    public RestBasicEntity() {
        uriBuilder = null;
    }

    @Override
    public boolean supports(MediaType mediaType) {
        return MediaType.APPLICATION_JSON_TYPE.equals(mediaType)
            || MediaType.APPLICATION_XML_TYPE.equals(mediaType);
    }

    /**
     * @param baseUri the base uri of the REST request
     * @throws KException if error occurs
     */
    public RestBasicEntity(URI baseUri) throws KException {
        ArgCheck.isNotNull(baseUri, "baseUri"); //$NON-NLS-1$
        setBaseUri(baseUri);
    }

    /**
     * @param baseUri the base uri of the REST request
     * @param kObject the kObject
     * @param uow the transaction
     * @param createCommonLinks should the self and parent links be created
     * @throws KException if error occurs
     */
    protected RestBasicEntity(URI baseUri, KomodoObject kObject, UnitOfWork uow, boolean createCommonLinks) throws KException {
        this(baseUri);

        ArgCheck.isNotNull(kObject, "kObject"); //$NON-NLS-1$
        ArgCheck.isNotNull(uow, "uow"); //$NON-NLS-1$

        setId(kObject.getName(uow));
        setDataPath(kObject.getAbsolutePath());
        setkType(kObject.getTypeIdentifier(uow));
        setHasChildren(kObject.hasChildren(uow));

        if (createCommonLinks) {
            KomodoProperties properties = new KomodoProperties();
            properties.addProperty(SEARCH_PATH_PARAMETER, getDataPath());
            addLink(new RestLink(LinkType.SELF, getUriBuilder().searchUri(properties)));

            KomodoObject parent = kObject.getParent(uow);
            ArgCheck.isNotNull(parent);
            properties = new KomodoProperties();
            properties.addProperty(SEARCH_PATH_PARAMETER, parent.getAbsolutePath());
            addLink(new RestLink(LinkType.PARENT, getUriBuilder().searchUri(properties)));

            createChildLink();
        }
    }

    protected void createChildLink() {
        KomodoProperties properties;
        properties = new KomodoProperties();
        properties.addProperty(SEARCH_PARENT_PARAMETER, getDataPath());
        addLink(new RestLink(LinkType.CHILDREN, getUriBuilder().searchUri(properties)));
    }

    /**
     * @param baseUri the base uri of the REST request
     * @param kObject the kObject
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestBasicEntity(URI baseUri, KomodoObject kObject, UnitOfWork uow) throws KException {
        this(baseUri, kObject, uow, true);
    }

    /**
     * @param kObject the object
     * @param parentClass the class of the desired parent
     * @return the ancestor of the object with the given class
     */
    protected <T extends KomodoObject> T ancestor(KomodoObject kObject, Class<T> parentClass, UnitOfWork uow) throws KException {
        WorkspaceManager wsMgr = WorkspaceManager.getInstance(kObject.getRepository(), uow);
        KomodoObject parent = kObject.getParent(uow);
        while (parent != null) {
            T resolvedParent = wsMgr.resolve(uow, parent, parentClass);
            if (resolvedParent != null)
                return resolvedParent;

            parent = parent.getParent(uow);
        }

        return null;
    }

    /**
     * @return the tuples
     */
    public Map<String, Object> getTuples() {
        return Collections.unmodifiableMap(this.tuples);
    }

    /**
     * @param key the key
     * @param value the value
     */
    public void addTuple(String key, Object value) {
        tuples.put(key, value);
    }

    /**
     * @return the base Uri of this entity
     */
    public URI getBaseUri() {
        Object uri = tuples.get(BASE_URI);
        return uri != null ? UriBuilder.fromUri(uri.toString()).build() : null;
    }

    /**
     * @param baseUri the base uri
     */
    public void setBaseUri(URI baseUri) {
        tuples.put(BASE_URI, baseUri);
        this.uriBuilder = new KomodoRestUriBuilder(baseUri);
    }

    /**
     * @return the uriBuilder
     */
    public KomodoRestUriBuilder getUriBuilder() {
        return this.uriBuilder;
    }

    /**
     * @return the id
     */
    public String getId() {
        Object id = tuples.get(ID);
        return id != null ? id.toString() : null;
    }

    /**
     * @param id the id to set
     */
    public void setId(String id) {
        tuples.put(ID, id);
    }

    /**
     * @return the dataPath
     */
    public String getDataPath() {
        Object path = tuples.get(DATA_PATH);
        return path != null ? path.toString() : null;
    }

    /**
     * @param dataPath the dataPath to set
     */
    public void setDataPath(String dataPath) {
        tuples.put(DATA_PATH, dataPath);
    }

    /**
     * @return the kType
     */
    public KomodoType getkType() {
        Object ktype = tuples.get(KTYPE);
        return ktype != null ? KomodoType.getKomodoType(ktype.toString()) : null;
    }

    /**
     * @param kType the kType to set
     */
    public void setkType(KomodoType kType) {
        tuples.put(KTYPE, kType);
    }

    /**
     * @return the hasChildren
     */
    public boolean hasChildren() {
        Object value = tuples.get(HAS_CHILDREN);
        return value != null ? Boolean.parseBoolean(value.toString()) : false;
    }

    /**
     * @param hasChildren the hasChildren to set
     */
    public void setHasChildren(boolean hasChildren) {
        tuples.put(HAS_CHILDREN, hasChildren);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        RestBasicEntity other = (RestBasicEntity)obj;
        if (this.links == null) {
            if (other.links != null)
                return false;
        } else
            if (!this.links.equals(other.links))
                return false;
        if (this.properties == null) {
            if (other.properties != null)
                return false;
        } else
            if (!this.properties.equals(other.properties))
                return false;
        if (this.tuples == null) {
            if (other.tuples != null)
                return false;
        } else
            if (!this.tuples.equals(other.tuples))
                return false;
        return true;
    }

    /**
     * @return the links (never <code>null</code> but can be empty)
     */
    public final Collection<RestLink> getLinks() {
        return this.links.values();
    }

    /**
     * @return the properties (never <code>null</code> but can be empty)
     */
    public final List<RestProperty> getProperties() {
        return this.properties;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.links == null) ? 0 : this.links.hashCode());
        result = prime * result + ((this.properties == null) ? 0 : this.properties.hashCode());
        result = prime * result + ((this.tuples == null) ? 0 : this.tuples.hashCode());
        return result;
    }

    /**
     * Adds a new link
     * @param newLink the new link
     */
    public final void addLink(RestLink newLink) {
        if (this.links == null || this.links == RestLink.NO_LINKS)
            this.links = new LinkedHashMap<>();

        this.links.put(newLink.getRel(), newLink);
    }

    /**
     * Removes the link of given type
     * @param type of link to remove
     */
    public final void removeLink(LinkType type) {
        if (this.links == null || this.links == RestLink.NO_LINKS)
            return;

        this.links.remove(type);
    }

    /**
     * @param newLinks
     *        the new links (can be <code>null</code>)
     */
    public final void setLinks(final Collection<RestLink> newLinks) {
        if (newLinks == null) {
            this.links = RestLink.NO_LINKS;
        } else {
            for (RestLink link : newLinks) {
                addLink(link);
            }
        }
    }

    /**
     * Add a property
     *
     * @param name the property name
     * @param value the property value
     */
    public final void addProperty(String name, Object value) {
        this.properties.add(new RestProperty(name, value));
    }

    /**
     * @param newProperties
     *        the new properties (can be <code>null</code>)
     */
    public final void setProperties(final List<RestProperty> newProperties) {
        this.properties.clear();

        if ((newProperties != null) && !newProperties.isEmpty()) {
            for (RestProperty property : newProperties) {
                ArgCheck.isNotNull(property);
                this.properties.add(property);
            }
        }
    }

    protected boolean hasPrefix(String name) {
        return name.matches(PREFIX_PATTERN);
    }

    /**
     * Derives execution properties from the given {@link KomodoObject}
     * and adds them to this entity
     *
     * @param uow transaction required for fetching the properties from the {@link KomodoObject}
     * @param kObject the source {@link KomodoObject}
     * @throws KException if error occurs
     */
    public void addExecutionProperties(UnitOfWork uow, KomodoObject kObject) throws KException {
        final List<String> propNames = new ArrayList<>(Arrays.asList(kObject.getPropertyNames(uow))); // props with values
        final PropertyDescriptor[] descriptors = kObject.getPropertyDescriptors(uow);

        if (descriptors.length != 0) {
            for (PropertyDescriptor descriptor : descriptors) {
                String name = descriptor.getName();
                if (!propNames.contains(name)) {
                    propNames.add(name);
                }
            }
        }

        //
        // Execution properties are stored in komodo object without a prefix
        //
        Iterator<String> propIter = propNames.iterator();
        while(propIter.hasNext()) {
            String propName = propIter.next();

            if (hasPrefix(propName))
                continue;

            Property attribute = kObject.getProperty(uow, propName);
            if (attribute == null)
                continue;

            if (attribute.isMultiple(uow)) {
                Object[] values = attribute.getValues(uow);
                addProperty(propName, values);
            } else {
                Object value = attribute.getValue(uow);
                addProperty(propName, value);
            }
        }
    }

    /**
     * @return the xml
     */
    @Override
    public String getXml() {
        return this.xml;
    }

    /**
     * @param xml the xml manifestation of this vdb
     */
    public void setXml(String xml) {
        this.xml = xml;
    }

    /**
     * @param instance new entity to clone into
     */
    public void clone(RestBasicEntity instance) {
        instance.tuples = this.tuples;
        instance.properties = this.properties;
        instance.links = this.links;
        instance.uriBuilder = this.uriBuilder;
        instance.xml = this.xml;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "RestBasicEntity [tuples=" + this.tuples + ", properties=" + this.properties + ", links=" + this.links + "]";
    }
}
