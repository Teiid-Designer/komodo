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
package org.komodo.repository.internal;

import org.modeshape.jcr.JcrRepository;

/**
 *
 */
public class WorkspaceIdentifier {

    private final String workspace;

    private JcrRepository repository;

    /**
     * Create a new instance
     * @param workspace the workspace name
     */
    public WorkspaceIdentifier(String workspace) {
        this.workspace = workspace;
    }

    /**
     * Create a new instance
     * @param workspace the workspace name
     * @param repository the repository
     */
    public WorkspaceIdentifier(String workspace, JcrRepository repository) {
        this.workspace = workspace;
        this.repository = repository;
    }

    /**
     * @return the workspace
     */
    public String getWorkspace() {
        return workspace;
    }

    /**
     * @return the repository
     */
    public JcrRepository getRepository() {
        return repository;
    }

    /**
     * @param repository the repository to set
     */
    public void setRepository(JcrRepository repository) {
        this.repository = repository;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.repository == null) ? 0 : this.repository.hashCode());
        result = prime * result + ((this.workspace == null) ? 0 : this.workspace.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        WorkspaceIdentifier other = (WorkspaceIdentifier)obj;
        if (this.repository == null) {
            if (other.repository != null)
                return false;
        } else if (!this.repository.equals(other.repository))
            return false;
        if (this.workspace == null) {
            if (other.workspace != null)
                return false;
        } else if (!this.workspace.equals(other.workspace))
            return false;
        return true;
    }
}
