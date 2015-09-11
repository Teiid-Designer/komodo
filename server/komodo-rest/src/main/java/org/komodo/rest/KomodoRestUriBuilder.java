/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest;

import static org.komodo.rest.KomodoRestV1Application.V1Constants.VDBS_URI_PATH;
import static org.komodo.rest.KomodoRestV1Application.V1Constants.VDB_MANIFEST_SEGMENT;
import java.net.URI;
import javax.ws.rs.core.UriBuilder;
import org.komodo.rest.json.RestLink.LinkType;
import org.komodo.utils.ArgCheck;

/**
 * Komodo REST URI builder.
 */
public final class KomodoRestUriBuilder {

    private final URI baseUri;

    /**
     * @param baseUri
     *        the base URI used when building URIs (cannot be <code>null</code>)
     */
    public KomodoRestUriBuilder( final URI baseUri ) {
        ArgCheck.isNotNull( baseUri, "baseUri" ); //$NON-NLS-1$
        this.baseUri = baseUri;
    }

    /**
     * @param linkType
     *        the type of URI being created (cannot be <code>null</code>)
     * @param vdbName
     *        the VDB name (cannot be empty)
     * @return the delete VDB URI for the specified VDB (never <code>null</code>)
     */
    public URI buildVdbUri( final LinkType linkType,
                            final String vdbName ) {
        ArgCheck.isNotNull( linkType, "linkType" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( vdbName, "vdbName" ); //$NON-NLS-1$

        URI result = null;

        switch ( linkType ) {
            case DELETE:
                result = getVdbDeleteUri( vdbName );
                break;
            case MANIFEST:
                result = getVdbManifestUri( vdbName );
                break;
            case PARENT:
                result = getVdbsUri();
                break;
            case SELF:
                result = getVdbUri( vdbName );
                break;
            default:
                throw new RuntimeException( "LinkType " + linkType + " not handled" ); //$NON-NLS-1$ //$NON-NLS-2$
        }

        assert( result != null );
        return result;
    }

    private URI getVdbDeleteUri( final String vdbName ) {
        return getVdbUri( vdbName );
    }

    private URI getVdbManifestUri( final String vdbName ) {
        return UriBuilder.fromUri( getVdbUri( vdbName ) ).path( VDB_MANIFEST_SEGMENT ).build();
    }

    /**
     * @return the URI to use when requesting a collection of VDBs in the workspace (never <code>null</code>)
     */
    public URI getVdbsUri() {
        return UriBuilder.fromUri( this.baseUri ).path( VDBS_URI_PATH ).build();
    }

    private URI getVdbUri( final String vdbName ) {
        return UriBuilder.fromUri( getVdbsUri() ).path( vdbName ).build();
    }

}
