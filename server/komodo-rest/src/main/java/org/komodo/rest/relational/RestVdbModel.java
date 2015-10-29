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
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Model.Type;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoRestEntity;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.CoreLexicon;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * A VDB model that can be used by GSON to build a JSON document model representation.
 */
public final class RestVdbModel extends KomodoRestEntity {

    /**
     * Label used to describe name
     */
    public static final String DESCRIPTION_LABEL = KomodoService.encode(VdbLexicon.Model.DESCRIPTION);

    /**
     * Label used to describe model type
     */
    public static final String MODEL_TYPE_LABEL = KomodoService.encode(CoreLexicon.JcrId.MODEL_TYPE);

    /**
     * Label used to describe visible
     */
    public static final String VISIBLE_LABEL = KomodoService.encode(VdbLexicon.Model.VISIBLE);

    /**
     * Label used to describe metadata type
     */
    public static final String METADATA_TYPE_LABEL = KomodoService.encode(VdbLexicon.Model.METADATA_TYPE);

    private String description;

    // This is the mmcore:modelType - contains values such as PHYSICAL
    private Type modelType;

    private boolean visible;

    private String metadataType;

    private String ddl;

    /**
     * Constructor for use when deserializing
     */
    public RestVdbModel() {
        super();
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
    public RestVdbModel(URI baseUri, String id, String dataPath, KomodoType kType, boolean hasChildren, String parentVdb) {
        super(baseUri, id, dataPath, kType, hasChildren);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().buildVdbModelUri(LinkType.SELF, parentVdb, id)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().buildVdbModelUri(LinkType.PARENT, parentVdb, id)));
        addLink(new RestLink(LinkType.SOURCES, getUriBuilder().buildVdbModelUri(LinkType.SOURCES, parentVdb, id)));
    }

    /**
     * @return the description
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
     * @return the modelType
     */
    public Type getModelType() {
        return this.modelType;
    }

    /**
     * @param type the modelType to set
     */
    public void setModelType(Type type) {
        this.modelType = type;
    }

    /**
     * @return the visible
     */
    public boolean isVisible() {
        return this.visible;
    }

    /**
     * @param visible the visible to set
     */
    public void setVisible(boolean visible) {
        this.visible = visible;
    }

    /**
     * @return the metadataType
     */
    public String getMetadataType() {
        return this.metadataType;
    }

    /**
     * @param metadataType the metadataType to set
     */
    public void setMetadataType(String metadataType) {
        this.metadataType = metadataType;
    }

    /**
     * @return the ddl
     */
    public String getDdl() {
        return this.ddl;
    }

    /**
     * @param ddl the ddl to set
     */
    public void setDdl(String ddl) {
        this.ddl = ddl;
    }

    /**
     * @param model source model
     * @param parentVdb parent Vdb
     * @param baseUri base uri
     * @param uow the transaction
     * @return a new {@link RestVdbModel} based on the source {@link Model}
     * @throws KException if error occurs
     */
    public static RestVdbModel build(final Model model, final Vdb parentVdb, final URI baseUri, final UnitOfWork uow) throws KException {

        final String vdbName = parentVdb.getName(uow);
        final RestVdbModel entity = new RestVdbModel(baseUri, model.getName(uow), model.getAbsolutePath(), model.getTypeIdentifier(uow), model.hasChildren(uow), vdbName);

        entity.setDescription(model.getDescription(uow));
        entity.setModelType(model.getModelType(uow));
        entity.setVisible(model.isVisible(uow));
        entity.setMetadataType(model.getMetadataType(uow));

        entity.addExecutionProperties(uow, model);

        Properties properties = new Properties();
        String ddl = model.export(uow, properties);
        if (ddl == null)
            ddl = EMPTY_STRING;

        entity.setDdl(ddl);

        return entity;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.metadataType == null) ? 0 : this.metadataType.hashCode());
        result = prime * result + ((this.modelType == null) ? 0 : this.modelType.hashCode());
        result = prime * result + (this.visible ? 1231 : 1237);
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
        RestVdbModel other = (RestVdbModel)obj;
        if (this.metadataType == null) {
            if (other.metadataType != null)
                return false;
        } else
            if (!this.metadataType.equals(other.metadataType))
                return false;
        if (this.modelType == null) {
            if (other.modelType != null)
                return false;
        } else
            if (!this.modelType.equals(other.modelType))
                return false;
        if (this.visible != other.visible)
            return false;
        return true;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "RestVdbModel [modelType=" + this.modelType + ", visible=" + this.visible + ", metadataType=" + this.metadataType + ", id=" + this.id + ", dataPath=" + this.dataPath + ", kType=" + this.kType + ", hasChildren=" + this.hasChildren + ", properties=" + this.properties + ", links=" + this.links + "]";
    }
}
