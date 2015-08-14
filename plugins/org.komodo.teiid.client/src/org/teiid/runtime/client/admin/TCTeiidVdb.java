package org.teiid.runtime.client.admin;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidVdb;
import org.teiid.adminapi.Model;
import org.teiid.adminapi.VDB;
import org.teiid.adminapi.impl.VDBMetaData;
import org.teiid.adminapi.impl.VDBMetadataParser;
import org.teiid.core.util.ArgCheck;
import org.teiid.runtime.client.Messages;

/**
 *
 */
public class TCTeiidVdb implements TeiidVdb, Comparable<TCTeiidVdb> {
    
    private static final String PREVIEW = "preview"; //$NON-NLS-1$
    private static final String DEPLOYMENT_NAME = "deployment-name"; //$NON-NLS-1$
    
    private final VDB vdb;

    private final TeiidInstance teiidInstance;

    private final boolean isPreview;
    private final String deploymentName;

    public TCTeiidVdb( VDB vdb,
                     TeiidInstance teiidInstance ) {
        ArgCheck.isNotNull(vdb, "vdb"); //$NON-NLS-1$
        ArgCheck.isNotNull(teiidInstance, "teiidInstance"); //$NON-NLS-1$

        this.vdb = vdb;
        this.teiidInstance = teiidInstance;
        isPreview = Boolean.parseBoolean(vdb.getProperties().getProperty(PREVIEW));
        deploymentName = vdb.getProperties().getProperty(DEPLOYMENT_NAME);
    }

    /**
     * @return the teiidInstance
     */
    public TeiidInstance getTeiidInstance() {
        return this.teiidInstance;
    }

    /* (non-Javadoc)
     * @see org.teiid.designer.runtime.impl.ITeiidVdb#compareTo(org.teiid.designer.runtime.impl.TeiidVdb)
     */
    @Override
	public int compareTo( TCTeiidVdb vdb ) {
        ArgCheck.isNotNull(vdb, "vdb"); //$NON-NLS-1$
        return getName().compareTo(vdb.getName());
    }

    /* (non-Javadoc)
     * @see org.teiid.designer.runtime.impl.ITeiidVdb#equals(java.lang.Object)
     */
    @Override
    public boolean equals( Object obj ) {
        if (obj == null) return false;
        if (obj.getClass() != getClass()) return false;

        TeiidVdb other = (TeiidVdb)obj;

        if (getName().equals(other.getName())) return true;

        return false;
    }
    
    /* (non-Javadoc)
     * @see org.teiid.designer.runtime.impl.ITeiidVdb#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.getName() == null) ? 0 : this.getName().hashCode());
        return result;
    }

    /* (non-Javadoc)
     * @see org.teiid.designer.runtime.impl.ITeiidVdb#getName()
     */
    @Override
    public String getName() {
        return this.vdb.getName();
    }

    @Override
    public String getDeployedName() {
        return deploymentName;
    }

    /* (non-Javadoc)
     * @see org.teiid.designer.runtime.impl.ITeiidVdb#getVersion()
     */
    @Override
    public int getVersion() {
        return this.vdb.getVersion();
    }

    /* (non-Javadoc)
     * @see org.teiid.designer.runtime.impl.ITeiidVdb#isPreviewVdb()
     */
    @Override
    public boolean isPreviewVdb() {
        return isPreview;
    }

    /* (non-Javadoc)
     * @see org.teiid.designer.runtime.impl.ITeiidVdb#isXmlDeployment()
     */
    @Override
    public boolean isXmlDeployment() {
        boolean isXml = false;
        if(vdb instanceof VDBMetaData) {
            isXml = ((VDBMetaData)vdb).isXmlDeployment();
        }
        return isXml;
    }
    
    @Override
    public boolean isActive() {
        return vdb.getStatus().equals(VDB.Status.ACTIVE);
    }
    
    @Override
    public boolean isLoading() {
        return vdb.getStatus().equals(VDB.Status.LOADING);
    }
    
    @Override
    public boolean hasFailed() {
        return vdb.getStatus().equals(VDB.Status.FAILED);
    }
    
    @Override
    public boolean wasRemoved() {
        return vdb.getStatus().equals(VDB.Status.REMOVED);
    }

    @Override
    public List<String> getValidityErrors() {
        List<String> errors = vdb.getValidityErrors();
        
        if (errors != null)
            return Collections.unmodifiableList(errors);
        
        return Collections.emptyList();
    }

    @Override
    public boolean hasModels() {
        return !vdb.getModels().isEmpty();
    }
    
    @Override
    public Collection<String> getModelNames() {
        if (! hasModels())
            return Collections.emptyList();
        
        List<String> names = new ArrayList<String>();
        for (Model model : vdb.getModels()) {
            names.add(model.getName());
        }
        
        return names;
    }

    @Override
    public String getPropertyValue(String key) {
        return vdb.getPropertyValue(key);
    }

    @Override
    public Properties getProperties( ) {
        return vdb.getProperties();
    }
    
    @Override
    public String export() throws Exception {
        String vdbStr = null;
        if(vdb instanceof VDBMetaData) {
            VDBMetaData vdbMeta = (VDBMetaData)vdb;
            if(vdbMeta.isXmlDeployment()) {
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                VDBMetadataParser.marshell((VDBMetaData)vdb, out);

                vdbStr = new String(out.toByteArray());
            } else {
                throw new Exception(Messages.getString(Messages.TeiidVdb.canOnlyExportDynamicVdbs));
            }
        }
        return vdbStr;
    }
}
