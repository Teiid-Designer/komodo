/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.spi.repository;

/**
 * The artifact descriptor.
 */
public interface ArtifactDescriptor {

    /**
     * An empty array of artifact descriptors.
     */
    ArtifactDescriptor[] EMPTY = new ArtifactDescriptor[0];

    /**
     * @return the type of the artifact (never empty)
     */
    String getArtifactType();

    /**
     * @return the artifact description (never empty)
     */
    String getDescription();

    /**
     * @return the artifact path in the repository library (never empty)
     */
    String getPath();

    /**
     * @return the repository where the artifact is located (never <code>null</code>)
     */
    Repository getRepository();

    /**
     * @return the version of the artifact (never empty)
     */
    String getVersion();

    /**
     * @return <code>true</code> if the artifact cannot be modified
     */
    boolean isReadOnly();

}
