/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import org.komodo.utils.ArgCheck;

/**
 * Indicates the objects has a JSON representation.
 */
public abstract class KomodoRestEntity {

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
        public ResourceNotFound( final String resourceName,
                                 final String operationName ) {
            ArgCheck.isNotEmpty( resourceName, "resourceName" ); //$NON-NLS-1$
            ArgCheck.isNotEmpty( operationName, "operationName" ); //$NON-NLS-1$

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

    protected final Map< String, String > properties = new HashMap< >();
    protected RestLink[] links = RestLink.NO_LINKS;

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object other ) {
        if ( ( other == null ) || !getClass().equals( other.getClass() ) ) {
            return false;
        }

        final KomodoRestEntity that = ( KomodoRestEntity )other;
        return ( this.properties.equals( that.properties ) && Arrays.deepEquals( this.links, that.links ) );
    }

    /**
     * @return the links (never <code>null</code> but can be empty)
     */
    public final RestLink[] getLinks() {
        return this.links;
    }

    /**
     * @return the properties (never <code>null</code> but can be empty)
     */
    public final Map< String, String > getProperties() {
        return this.properties;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return Objects.hash( this.properties, Arrays.deepHashCode( this.links ) );
    }

    /**
     * @param newLinks
     *        the new links (can be <code>null</code>)
     */
    public final void setLinks( final RestLink[] newLinks ) {
        if ( newLinks == null ) {
            this.links = RestLink.NO_LINKS;
        } else {
            this.links = newLinks;
        }
    }

    /**
     * @param newProperties
     *        the new properties (can be <code>null</code>)
     */
    public final void setProperties( final Map< String, String > newProperties ) {
        this.properties.clear();

        if ( ( newProperties != null ) && !newProperties.isEmpty() ) {
            this.properties.putAll( newProperties );
        }
    }

}
