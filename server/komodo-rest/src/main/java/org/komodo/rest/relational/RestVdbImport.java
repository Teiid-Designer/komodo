/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import java.net.URI;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.VdbImport;
import org.komodo.rest.KomodoRestEntity;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * A VDB import that can be used by GSON to build a JSON document representation.
 *
 * <pre>
 * <code>
 * {
 *     "id" : "MyImportVdb",
 *     "version" : "1",
 *     "importDataPolicies" : "true"
 * }
 * </code>
 * </pre>
 */
public final class RestVdbImport extends KomodoRestEntity {

    /**
     * Label used to describe name
     */
    public static final String NAME_LABEL = KomodoService.encode(VdbLexicon.ImportVdb.IMPORT_VDB);

    /**
     * Label used to describe import data policies
     */
    public static final String IMPORT_POLICIES_LABEL = KomodoService.encode(VdbLexicon.ImportVdb.IMPORT_DATA_POLICIES);

    /**
     * Label used to describe version
     */
    public static final String VERSION_LABEL = KomodoService.encode(VdbLexicon.ImportVdb.VERSION);

    /**
     * And empty array of VDB imports.
     */
    public static final RestVdbImport[] NO_IMPORTS = new RestVdbImport[ 0 ];

    private String name;
    private int version = Vdb.DEFAULT_VERSION;
    private boolean importDataPolicies = VdbImport.DEFAULT_IMPORT_DATA_POLICIES;

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbImport() {
        // nothing to do
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the vdb
     * @param id the id of this vdb
     * @param dataPath the data path of this vdb
     * @param kType the type of this vdb
     * @param hasChildren true if vdb has children
     * @param parentVdb the name of the parent vdb
     */
    public RestVdbImport(URI baseUri, String id, String dataPath, KomodoType kType, boolean hasChildren, String parentVdb) {
        super(baseUri, id, dataPath, kType, hasChildren);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().buildVdbImportUri(LinkType.SELF, parentVdb, id)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().buildVdbImportUri(LinkType.PARENT, parentVdb, id)));
    }

    /**
     * @return the name (can be empty)
     */
    public String getName() {
        return this.name;
    }

    /**
     * @param newName
     *        the new VDB import name (can be empty)
     */
    public void setName( final String newName ) {
        this.name = newName;
    }

    /**
     * @return the VDB version
     */
    public int getVersion() {
        return this.version;
    }

    /**
     * @param newVersion
     *        the new VDB import version
     */
    public void setVersion( final int newVersion ) {
        this.version = newVersion;
    }

    /**
     * @return <code>true</code> if importing data policies
     */
    public boolean isImportDataPolicies() {
        return this.importDataPolicies;
    }

    /**
     * @param newImportDataPolicies
     *        the new import data policies
     */
    public void setImportDataPolicies( final boolean newImportDataPolicies ) {
        this.importDataPolicies = newImportDataPolicies;
    }

    /**
     * @param vdbImport the source vdb import
     * @param parentVdb the parent vdb
     * @param baseUri base uri
     * @param uow the transaction
     * @return a new {@link RestVdbImport} based on the source {@link VdbImport}
     * @throws KException if error occurs
     */
    public static RestVdbImport build(VdbImport vdbImport, Vdb parentVdb, URI baseUri, UnitOfWork uow) throws KException {
        final String vdbName = parentVdb.getName(uow);
        final RestVdbImport entity = new RestVdbImport(baseUri,
                                                                                       vdbImport.getName(uow),
                                                                                       vdbImport.getAbsolutePath(),
                                                                                       vdbImport.getTypeIdentifier(uow),
                                                                                       vdbImport.hasChildren(uow), vdbName);

        entity.setName(vdbImport.getName(uow));
        entity.setVersion(vdbImport.getVersion(uow));
        entity.setImportDataPolicies(vdbImport.isImportDataPolicies(uow));

        return entity;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.importDataPolicies ? 1231 : 1237);
        result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
        result = prime * result + this.version;
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        RestVdbImport other = (RestVdbImport)obj;
        if (this.importDataPolicies != other.importDataPolicies)
            return false;
        if (this.name == null) {
            if (other.name != null)
                return false;
        } else
            if (!this.name.equals(other.name))
                return false;
        if (this.version != other.version)
            return false;
        return true;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "RestVdbImport [name=" + this.name + ", version=" + this.version + ", importDataPolicies=" + this.importDataPolicies + ", id=" + this.id + ", dataPath=" + this.dataPath + ", kType=" + this.kType + ", hasChildren=" + this.hasChildren + ", properties=" + this.properties + ", links=" + this.links + "]";
    }

}
