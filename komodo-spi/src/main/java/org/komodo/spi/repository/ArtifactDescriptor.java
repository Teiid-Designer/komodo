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
