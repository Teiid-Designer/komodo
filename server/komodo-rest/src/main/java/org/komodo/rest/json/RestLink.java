/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import java.net.URI;
import javax.ws.rs.core.UriBuilder;
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
         * A content link.
         */
        CONTENT,

        /**
         * A link to the parent resource.
         */
        PARENT,

        /**
         * A link to its own resource.
         */
        SELF;

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

            throw new RuntimeException();
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

    private final String rel;
    private final String href;

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

        this.rel = rel.toString();
        this.href = href.toString();
    }

    /**
     * @return the href the HREF (never <code>null</code>)
     */
    public URI getHref() {
        return UriBuilder.fromUri( this.href ).build();
    }

    /**
     * @return the rel the link type (never <code>null</code>)
     */
    public LinkType getRel() {
        return LinkType.fromString( this.rel );
    }

}
