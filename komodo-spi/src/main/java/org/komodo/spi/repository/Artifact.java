/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.spi.repository;

/**
 * The Komodo library artifact.
 */
public interface Artifact {

    /**
     * @return the artifact's Komodo object (never <code>null</code>)
     */
    KomodoObject get();

    /**
     * @return the artifact descriptor (never <code>null</code>)
     */
    ArtifactDescriptor getDescriptor();

}
