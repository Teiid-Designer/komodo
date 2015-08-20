/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest;

import static org.komodo.rest.KomodoRestV1Application.V1Constants.VDBS_URI_PATH;
import java.net.URI;
import javax.ws.rs.core.UriBuilder;
import org.komodo.utils.ArgCheck;

/**
 * Komodo REST utilities.
 */
public final class KomodoRestUtils {

    private static final String VDB_CONTENT_EXTENSION = ".xml"; //$NON-NLS-1$

    /**
     * @param baseUri
     *        the base URI (cannot be <code>null</code>)
     * @param vdbName
     *        the VDB name (cannot be empty)
     * @return the VDB content URI for the specified VDB (never <code>null</code>)
     */
    public static URI getVdbContentUri( final URI baseUri,
                                        final String vdbName ) {
        ArgCheck.isNotEmpty( vdbName, "vdbName" ); //$NON-NLS-1$
        return UriBuilder.fromUri( getVdbUri( baseUri, vdbName + VDB_CONTENT_EXTENSION ) ).build();
    }

    /**
     * @param baseUri
     *        the base URI (cannot be <code>null</code>)
     * @return the URI for the collection of VDBs in the workspace (never <code>null</code>)
     */
    public static URI getVdbsUri( final URI baseUri ) {
        ArgCheck.isNotNull( baseUri, "baseUri" ); //$NON-NLS-1$
        return UriBuilder.fromUri( baseUri ).path( VDBS_URI_PATH ).build();
    }

    /**
     * @param baseUri
     *        the base URI (cannot be <code>null</code>)
     * @param vdbName
     *        the VDB name (cannot be empty)
     * @return the URI for the specified VDB (never <code>null</code>)
     */
    public static URI getVdbUri( final URI baseUri,
                                 final String vdbName ) {
        ArgCheck.isNotEmpty( vdbName, "vdbName" ); //$NON-NLS-1$
        return UriBuilder.fromUri( getVdbsUri( baseUri ) ).path( vdbName ).build();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private KomodoRestUtils() {
        // nothing to do
    }

}
