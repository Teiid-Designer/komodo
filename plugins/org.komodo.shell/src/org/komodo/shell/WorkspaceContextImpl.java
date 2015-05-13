/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.shell;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.RepositoryTools;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceContextVisitor;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;

/**
 * The WorkspaceContext
 */
public class WorkspaceContextImpl implements WorkspaceContext {

    private static String[] RELATIONAL_TYPES = {
        KomodoLexicon.Schema.NODE_TYPE,
        KomodoLexicon.Vdb.NODE_TYPE,
        KomodoLexicon.VdbModel.NODE_TYPE
    };
    private static List<String> relationalTypes = Arrays.asList(RELATIONAL_TYPES);

    private final WorkspaceStatus wsStatus;
    private final WorkspaceContext parent;
    private final KomodoObject repoObject;

    /**
     * @param wsStatus the workspace status object
     * @param parent the parent context
     * @param repoObject repository object on which this context is based
     */
    public WorkspaceContextImpl( WorkspaceStatus wsStatus,
                                 WorkspaceContext parent,
                                 KomodoObject repoObject ) {
        super();
        this.wsStatus = wsStatus;
        this.parent = parent;
        this.repoObject = repoObject;
    }

    @Override
    public Repository getRepository() throws Exception {
        return repoObject.getRepository();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.parent == null) ? 0 : this.parent.hashCode());
        result = prime * result + ((this.repoObject == null) ? 0 : this.repoObject.hashCode());
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
        WorkspaceContextImpl other = (WorkspaceContextImpl)obj;
        if (this.parent == null) {
            if (other.parent != null)
                return false;
        } else if (!this.parent.equals(other.parent))
            return false;
        if (this.repoObject == null) {
            if (other.repoObject != null)
                return false;
        } else if (!this.repoObject.equals(other.repoObject))
            return false;
        return true;
    }


    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceContext#getName()
     */
    @Override
    public String getName() throws Exception {
        return this.repoObject.getName(this.wsStatus.getTransaction());
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceContext#getType()
     */
    @Override
    public String getType() throws Exception {
        return this.repoObject.getTypeIdentifier(this.wsStatus.getTransaction()).name();
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceContext#getParent()
     */
    @Override
    public WorkspaceContext getParent() {
        return this.parent;
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceContext#getChildren()
     */
    @Override
    public List<WorkspaceContext> getChildren() throws Exception {
        List<WorkspaceContext> childrenCtx = new ArrayList<WorkspaceContext>();
        KomodoObject[] children = repoObject.getChildren(this.wsStatus.getTransaction());

        for (KomodoObject child : children) {
            childrenCtx.add(new WorkspaceContextImpl(wsStatus, this, child));
        }

        return childrenCtx;
    }

    private WorkspaceContext createWorkspaceContext(KomodoObject relObj) throws Exception {
        WorkspaceContext context = wsStatus.getWorkspaceContext(relObj.getAbsolutePath());
        if (context == null) {
            context = new WorkspaceContextImpl(wsStatus, this, relObj);
            wsStatus.addWorkspaceContext(relObj.getAbsolutePath(), context);
        }

        return context;
    }

    /**
     * Get the full name path for this context.  e.g. home.parentContext.thisContext
     * @return the full name
     * @throws Exception if error occurs
     */
    @Override
    public String getFullName() throws Exception {
        WorkspaceContext parentContext = this.parent;
        StringBuffer sb = new StringBuffer();

        // Ensures a slash is placed right at the root of the path
        if (parentContext != null)
            sb.append(parentContext.getFullName());

        sb.append(File.separator);
        sb.append(getName());

        return sb.toString();
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceContext#isRelational()
     */
    @Override
    public boolean isRelational() {
        try {
            return relationalTypes.contains(getType());
        } catch (Exception ex) {
            return false;
        }
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceContext#getChild(java.lang.String, org.komodo.shell.api.WorkspaceContext.Type)
     */
    @Override
    public WorkspaceContext getChild(String name) throws Exception {
        KomodoObject[] children = repoObject.getChildren(this.wsStatus.getTransaction());

        for (KomodoObject child : children) {
            String childName = child.getName(this.wsStatus.getTransaction());
            if (childName.equalsIgnoreCase(name)) {
                return createWorkspaceContext(child);
            }
        }

        return null;
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceContext#getChild(java.lang.String, org.komodo.shell.api.WorkspaceContext.Type)
     */
    @Override
    public WorkspaceContext getChild(String name, String type) throws Exception {
        KomodoObject[] children = repoObject.getChildrenOfType(this.wsStatus.getTransaction(), type);

        for (KomodoObject child : children) {
            String childName = child.getName(this.wsStatus.getTransaction());
            if (childName.equalsIgnoreCase(name)) {
                return createWorkspaceContext(child);
            }
        }

        return null;
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceContext#getPropertyNameValueMap()
     */
    @Override
    public List<String> getProperties() throws Exception {
////        if (getType().equals(WorkspaceContext.Type.ROOT)) {
////            propNameValues.put(WorkspaceStatus.RECORDING_FILE_KEY, getWorkspaceStatus().getRecordingOutputFile().toString());

        KomodoObject relObj = getKomodoObj();
        String[] props = relObj.getPropertyNames(this.wsStatus.getTransaction());
        return Arrays.asList(props);
    }

    @Override
    public String getPropertyValue(String propertyName) throws Exception {
        KomodoObject relObj = getKomodoObj();
        Property property = relObj.getProperty(this.wsStatus.getTransaction(), propertyName);
        // Null means the property with that name wasn't found
        if(property==null) {
        	return null;
        } else {
        	return RepositoryTools.getDisplayValue(this.wsStatus.getTransaction(), property);
        }
    }

    @Override
    public void setPropertyValue(String propertyName, Object value) throws Exception {
        KomodoObject relObj = getKomodoObj();
        relObj.setProperty(this.wsStatus.getTransaction(), propertyName, value);
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceContext#getRelationalObj()
     */
    @Override
    public KomodoObject getKomodoObj() {
        return repoObject;
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceContext#getWorkspaceStatus()
     */
    @Override
    public WorkspaceStatus getWorkspaceStatus() {
        return this.wsStatus;
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.getClass().getName());

        try {
            sb.append(" : name = ").append(getFullName()); //$NON-NLS-1$
        } catch (Exception ex) {
            sb.append(ex.getMessage());
        }

        return sb.toString();
    }

    @Override
    public Object visit(WorkspaceContextVisitor visitor) throws Exception {
        return visitor.visit(this);
    }

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.WorkspaceContext#getWorkspaceManager()
	 */
	@Override
	public WorkspaceManager getWorkspaceManager() throws Exception {
        return WorkspaceManager.getInstance(getRepository());
	}
}
