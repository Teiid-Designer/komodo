/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import java.net.URI;
import java.util.Properties;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestEntity;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * A VDB that can be used by GSON to build a JSON document representation.
 */
public final class RestVdb extends KomodoRestEntity {

    /**
     * Label used to describe name
     */
    public static final String NAME_LABEL = KomodoService.encode(VdbLexicon.Vdb.NAME);

    /**
     * Label used to describe description
     */
    public static final String DESCRIPTION_LABEL = KomodoService.encode(VdbLexicon.Vdb.DESCRIPTION);

    /**
     * Label used to describe original file path
     */
    public static final String FILE_PATH_LABEL = KomodoService.encode(VdbLexicon.Vdb.ORIGINAL_FILE);

    /**
     * Label used to describe original file path
     */
    public static final String PREVIEW_LABEL = KomodoService.encode(VdbLexicon.Vdb.PREVIEW);

    /**
     * Label used to describe original file path
     */
    public static final String CONNECTION_TYPE_LABEL = KomodoService.encode(VdbLexicon.Vdb.CONNECTION_TYPE);

    /**
     * Label used to describe original file path
     */
    public static final String VERSION_LABEL = KomodoService.encode(VdbLexicon.Vdb.VERSION);

    private String name;

    private String description;

    private String originalFilePath;

    private boolean preview;

    private String connectionType;

    private int version;

    /**
     * Constructor for use when deserializing
     */
    public RestVdb() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the vdb
     * @param id the id of this vdb
     * @param dataPath the data path of this vdb
     * @param kType the type of this vdb
     * @param hasChildren true if vdb has children
     */
    public RestVdb(URI baseUri, String id, String dataPath, KomodoType kType, boolean hasChildren) {
        super(baseUri, id, dataPath, kType, hasChildren);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().buildVdbUri(LinkType.SELF, id)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().buildVdbUri(LinkType.PARENT, id)));
        addLink(new RestLink(LinkType.IMPORTS, getUriBuilder().buildVdbUri(LinkType.IMPORTS, id)));
        addLink(new RestLink(LinkType.MODELS, getUriBuilder().buildVdbUri(LinkType.MODELS, id)));
        addLink(new RestLink(LinkType.TRANSLATORS, getUriBuilder().buildVdbUri(LinkType.TRANSLATORS, id)));
        addLink(new RestLink(LinkType.DATA_ROLES, getUriBuilder().buildVdbUri(LinkType.DATA_ROLES, id)));
    }

    /**
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    /**
     * @param name the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the VDB description (can be empty)
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * @param description the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * @return the originalFilePath
     */
    public String getOriginalFilePath() {
        return this.originalFilePath;
    }

    /**
     * @param originalFilePath the originalFilePath to set
     */
    public void setOriginalFilePath(String originalFilePath) {
        this.originalFilePath = originalFilePath;
    }

    /**
     * @return the preview
     */
    public boolean isPreview() {
        return this.preview;
    }

    /**
     * @param preview the preview to set
     */
    public void setPreview(boolean preview) {
        this.preview = preview;
    }

    /**
     * @return the connectionType
     */
    public String getConnectionType() {
        return this.connectionType;
    }

    /**
     * @param connectionType the connectionType to set
     */
    public void setConnectionType(String connectionType) {
        this.connectionType = connectionType;
    }

    /**
     * @return the version
     */
    public int getVersion() {
        return this.version;
    }

    /**
     * @param version the version to set
     */
    public void setVersion(int version) {
        this.version = version;
    }

    /**
     * @param vdb the source vdb
     * @param exportAsXml whether to export the xml
     * @param baseUri the base uri
     * @param uow the transaction
     * @return a new {@link RestVdb} from the given source {@link Vdb}
     * @throws KException if error occurs
     */
    public static RestVdb build(final Vdb vdb, final boolean exportAsXml, final URI baseUri, final UnitOfWork uow) throws KException {
        final String vdbName = vdb.getName(uow);
        final RestVdb restVdb = new RestVdb(baseUri, vdbName, vdb.getAbsolutePath(), vdb.getTypeIdentifier(uow), vdb.hasChildren(uow));

        restVdb.setName(vdb.getName(uow));
        restVdb.setDescription(vdb.getDescription(uow));
        restVdb.setOriginalFilePath(vdb.getOriginalFilePath(uow));

        restVdb.setPreview(vdb.isPreview(uow));
        restVdb.setConnectionType(vdb.getConnectionType(uow));
        restVdb.setVersion(vdb.getVersion(uow));

        restVdb.addExecutionProperties(uow, vdb);

        if (exportAsXml) {
            String xml = vdb.export(uow, new Properties());
            restVdb.setXml(xml);
        }

        return restVdb;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.connectionType == null) ? 0 : this.connectionType.hashCode());
        result = prime * result + ((this.description == null) ? 0 : this.description.hashCode());
        result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
        result = prime * result + ((this.originalFilePath == null) ? 0 : this.originalFilePath.hashCode());
        result = prime * result + (this.preview ? 1231 : 1237);
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
        RestVdb other = (RestVdb)obj;
        if (this.connectionType == null) {
            if (other.connectionType != null)
                return false;
        } else
            if (!this.connectionType.equals(other.connectionType))
                return false;
        if (this.description == null) {
            if (other.description != null)
                return false;
        } else
            if (!this.description.equals(other.description))
                return false;
        if (this.name == null) {
            if (other.name != null)
                return false;
        } else
            if (!this.name.equals(other.name))
                return false;
        if (this.originalFilePath == null) {
            if (other.originalFilePath != null)
                return false;
        } else
            if (!this.originalFilePath.equals(other.originalFilePath))
                return false;
        if (this.preview != other.preview)
            return false;
        if (this.version != other.version)
            return false;
        return true;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "RestVdb [name=" + this.name + ", description=" + this.description + ", originalFilePath=" + this.originalFilePath + ", preview=" + this.preview + ", connectionType=" + this.connectionType + ", version=" + this.version + ", id=" + this.id + ", dataPath=" + this.dataPath + ", kType=" + this.kType + ", hasChildren=" + this.hasChildren + ", properties=" + this.properties + ", links=" + this.links + "]";
    }
}
