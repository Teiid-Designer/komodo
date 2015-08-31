/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import java.net.URI;
import java.util.Objects;
import org.komodo.utils.ArgCheck;

/**
 * Represents a navigable web link.
 */
public final class RestLink {

    /**
     * The link relationship type.
     */
    public enum LinkType {

        /**
         * A delete resource link.
         */
        DELETE,

        /**
         * A resource content link to the VDB XML manifest.
         */
        MANIFEST,

        /**
         * A link to the parent resource.
         */
        PARENT,

        /**
         * A link to its own resource.
         */
        SELF;

        /**
         * An empty array of link types.
         */
        public static final LinkType[] NO_LINK_TYPES = new LinkType[ 0 ];

        /**
         * @param text
         *        the text whose enum is being requested (can be empty)
         * @return the link type (never <code>null</code>)
         * @throws RuntimeException
         *         if text is empty or does not represent a link type
         */
        public static LinkType fromString( final String text ) {
            for ( final LinkType type : values() ) {
                if ( type.toString().equals( text ) ) {
                    return type;
                }
            }

            throw new RuntimeException( "Unexpected link type of '" + text + '\'' ); //$NON-NLS-1$
        }

        /**
         * {@inheritDoc}
         *
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return name().toLowerCase();
        }

    }

    /**
     * The HTTP methods associated with the link.
     */
    public enum MethodType {

        /**
         * Delete a resource.
         */
        DELETE,

        /**
         * Retrieve a resource.
         */
        GET,

        /**
         * Patch a resource.
         */
        PATCH,

        /**
         * Add or update a resource.
         */
        POST,

        /**
         * Create a resource.
         */
        PUT;

        /**
         * @param text
         *        the text whose enum is being requested (can be empty)
         * @return the link type (never <code>null</code>)
         * @throws RuntimeException
         *         if text is empty or does not represent a link type
         */
        public static MethodType fromString( final String text ) {
            for ( final MethodType type : values() ) {
                if ( type.toString().equals( text ) ) {
                    return type;
                }
            }

            throw new RuntimeException( "Unexpected method type of '" + text + '\'' ); //$NON-NLS-1$
        }
    }

    /**
     * An empty array of links.
     */
    public static final RestLink[] NO_LINKS = new RestLink[ 0 ];

    private LinkType rel;
    private URI href;
    private MethodType method;

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestLink() {
        // nothing to do
    }

    /**
     * @param rel
     *        the link type (cannot be <code>null</code>)
     * @param href
     *        the link HREF (cannot be <code>null</code>)
     */
    public RestLink( final LinkType rel,
                     final URI href ) {
        ArgCheck.isNotNull( rel, "rel" ); //$NON-NLS-1$
        ArgCheck.isNotNull( href, "href" ); //$NON-NLS-1$

        this.rel = rel;
        this.href = href;

        MethodType type = null;

        switch ( rel ) {
            case DELETE:
                type = MethodType.DELETE;
                break;
            case MANIFEST:
                type = MethodType.GET;
                break;
            case PARENT:
                type = MethodType.GET;
                break;
            case SELF:
                type = MethodType.GET;
                break;
            default:
                throw new RuntimeException( "Unexpected link type of '" + rel + '\'' ); //$NON-NLS-1$
        }

        this.method = type;
    }

    /**
     * @param rel
     *        the link type (cannot be <code>null</code>)
     * @param href
     *        the link HREF (cannot be <code>null</code>)
     * @param method
     *        the method type (cannot be <code>null</code>)
     */
    public RestLink( final LinkType rel,
                     final URI href,
                     final MethodType method ) {
        ArgCheck.isNotNull( rel, "rel" ); //$NON-NLS-1$
        ArgCheck.isNotNull( href, "href" ); //$NON-NLS-1$
        ArgCheck.isNotNull( method, "method" ); //$NON-NLS-1$

        this.rel = rel;
        this.href = href;
        this.method = method;
    }

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

        final RestLink that = ( RestLink )other;

        // rel
        if ( this.rel != that.rel ) {
            return false;
        }

        // href
        if ( this.href == null ) {
            if ( that.href != null ) {
                return false;
            }
        } else if ( !this.href.equals( that.href ) ) {
            return false;
        }

        // method type
        if ( this.method != that.method ) {
            return false;
        }

        return true;
    }

    /**
     * @return the href the HREF (can be <code>null</code>)
     */
    public URI getHref() {
        return this.href;
    }

    /**
     * @return the method type (can be <code>null</code>)
     */
    public MethodType getMethod() {
        return this.method;
    }

    /**
     * @return the rel link type (can be <code>null</code>)
     */
    public LinkType getRel() {
        return this.rel;
    }

    /**
     * {@inheritDoc}
     *
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        return Objects.hash( this.rel, this.href, this.method );
    }

    /**
     * @param newHref
     *        the new HREF (can be <code>null</code>)
     */
    public void setHref( final URI newHref ) {
        this.href = newHref;
    }

    /**
     * @param newMethodType
     *        the new method type (can be <code>null</code>)
     */
    public void setMethod( final MethodType newMethodType ) {
        this.method = newMethodType;
    }

    /**
     * @param newRel
     *        the new link type (can be <code>null</code>)
     */
    public void setRel( final LinkType newRel ) {
        this.rel = newRel;
    }

}
