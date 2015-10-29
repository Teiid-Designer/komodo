/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import org.komodo.rest.json.JsonConstants;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

/**
 * Indicates the objects has a JSON representation.
 */
public abstract class KomodoRestEntity implements JsonConstants {

    /**
     * A {@link KomodoRestEntity} that indicates the resource was not found.
     */
    public static class ResourceNotFound extends KomodoRestEntity {

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
    public static final KomodoRestEntity NO_CONTENT = new KomodoRestEntity() {
        // nothing to do
    };

    private final transient KomodoRestUriBuilder uriBuilder;

    protected String id;

    protected String dataPath;

    protected KomodoType kType;

    protected boolean hasChildren;

    protected final List<KomodoRestProperty> properties = new ArrayList<>();

    protected List<RestLink> links = RestLink.NO_LINKS;

    // Transient to ensure its never serialized by Gson
    private transient String xml;

    /**
     * Used for NO_CONTENT and ResourceNotFound
     */
    protected KomodoRestEntity() {
        uriBuilder = null;
    }

    /**
     * @param baseUri the base uri of the REST request
     * @param id the id of this entity
     * @param dataPath the data path of this entity
     * @param kType the type of this entity
     * @param hasChildren true if entity has children
     */
    public KomodoRestEntity(URI baseUri, String id, String dataPath, KomodoType kType, boolean hasChildren) {
        ArgCheck.isNotNull(baseUri, "baseUri"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(id, "id"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(dataPath, "dataPath"); //$NON-NLS-1$
        ArgCheck.isNotNull(kType, "kType"); //$NON-NLS-1$

        this.uriBuilder = new KomodoRestUriBuilder(baseUri);
        this.id = id;
        this.dataPath = dataPath;
        this.kType = kType;
        this.hasChildren = hasChildren;
    }

    /**
     * @return the base Uri of this entity
     */
    public URI getBaseUri() {
        return this.uriBuilder != null ? this.uriBuilder.baseUri() : null;
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
        return this.id;
    }

    /**
     * @param id the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * @return the dataPath
     */
    public String getDataPath() {
        return this.dataPath;
    }

    /**
     * @param dataPath the dataPath to set
     */
    public void setDataPath(String dataPath) {
        this.dataPath = dataPath;
    }

    /**
     * @return the kType
     */
    public KomodoType getkType() {
        return this.kType;
    }

    /**
     * @param kType the kType to set
     */
    public void setkType(KomodoType kType) {
        this.kType = kType;
    }

    /**
     * @return the hasChildren
     */
    public boolean hasChildren() {
        return this.hasChildren;
    }

    /**
     * @param hasChildren the hasChildren to set
     */
    public void setHasChildren(boolean hasChildren) {
        this.hasChildren = hasChildren;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        KomodoRestEntity other = (KomodoRestEntity)obj;
        if (this.dataPath == null) {
            if (other.dataPath != null)
                return false;
        } else
            if (!this.dataPath.equals(other.dataPath))
                return false;
        if (this.hasChildren != other.hasChildren)
            return false;
        if (this.id == null) {
            if (other.id != null)
                return false;
        } else
            if (!this.id.equals(other.id))
                return false;
        if (this.kType != other.kType)
            return false;
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
        return true;
    }

    /**
     * @return the links (never <code>null</code> but can be empty)
     */
    public final List<RestLink> getLinks() {
        return this.links;
    }

    /**
     * @return the properties (never <code>null</code> but can be empty)
     */
    public final List<KomodoRestProperty> getProperties() {
        return this.properties;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.dataPath == null) ? 0 : this.dataPath.hashCode());
        result = prime * result + (this.hasChildren ? 1231 : 1237);
        result = prime * result + ((this.id == null) ? 0 : this.id.hashCode());
        result = prime * result + ((this.kType == null) ? 0 : this.kType.hashCode());
        result = prime * result + ((this.links == null) ? 0 : this.links.hashCode());
        result = prime * result + ((this.properties == null) ? 0 : this.properties.hashCode());
        return result;
    }

    /**
     * Adds a new link
     * @param newLink the new link
     */
    public final void addLink(RestLink newLink) {
        if (this.links == null || this.links == RestLink.NO_LINKS)
            this.links = new ArrayList<RestLink>();

        this.links.add(newLink);
    }

    /**
     * @param newLinks
     *        the new links (can be <code>null</code>)
     */
    public final void setLinks(final List<RestLink> newLinks) {
        if (newLinks == null) {
            this.links = RestLink.NO_LINKS;
        } else {
            this.links = newLinks;
        }
    }

    /**
     * Add a property
     *
     * @param name the property name
     * @param value the property value
     */
    public final void addProperty(String name, String value) {
        this.properties.add(new KomodoRestProperty(name, value));
    }

    /**
     * @param newProperties
     *        the new properties (can be <code>null</code>)
     */
    public final void setProperties(final List<KomodoRestProperty> newProperties) {
        this.properties.clear();

        if ((newProperties != null) && !newProperties.isEmpty()) {
            for (KomodoRestProperty property : newProperties) {
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
                addProperty(propName, StringUtils.toCommaSeparatedList(values));
            } else {
                Object value = attribute.getValue(uow);
                addProperty(propName, value == null ? EMPTY_STRING : value.toString());
            }
        }
    }

    /**
     * @return the xml
     */
    public String getXml() {
        return this.xml;
    }

    /**
     * @param xml the xml manifestation of this vdb
     */
    public void setXml(String xml) {
        this.xml = xml;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "KomodoRestEntity [id=" + this.id + ", dataPath=" + this.dataPath + ", kType=" + this.kType + ", properties=" + this.properties + "]";
    }

}
