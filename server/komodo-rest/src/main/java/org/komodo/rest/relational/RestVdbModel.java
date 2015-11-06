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
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.json.JsonConstants;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.CoreLexicon;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;
import org.teiid.core.util.ArgCheck;

/**
 * A VDB model that can be used by GSON to build a JSON document model representation.
 */
public final class RestVdbModel extends RestBasicEntity {

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

    /**
     * Constructor for use when deserializing
     */
    public RestVdbModel() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param model the model
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestVdbModel(URI baseUri, Model model, UnitOfWork uow) throws KException {
        super(baseUri, model, uow);

        setDescription(model.getDescription(uow));
        setModelType(model.getModelType(uow));
        setVisible(model.isVisible(uow));
        setMetadataType(model.getMetadataType(uow));

        addExecutionProperties(uow, model);

        Properties properties = new Properties();
        String ddl = model.export(uow, properties);
        if (ddl == null)
            ddl = EMPTY_STRING;

        setDdl(ddl);

        Vdb vdb = ancestor(model, Vdb.class, uow);
        ArgCheck.isNotNull(vdb);
        String vdbName = vdb.getName(uow);

        addLink(new RestLink(LinkType.SELF, getUriBuilder().buildVdbModelUri(LinkType.SELF, vdbName, getId())));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().buildVdbModelUri(LinkType.PARENT, vdbName, getId())));
        addLink(new RestLink(LinkType.SOURCES, getUriBuilder().buildVdbModelUri(LinkType.SOURCES, vdbName, getId())));
    }

    /**
     * @return the description
     */
    public String getDescription() {
        Object description = tuples.get(DESCRIPTION_LABEL);
        return description != null ? description.toString() : null;
    }

    /**
     * @param description the description to set
     */
    public void setDescription(String description) {
        tuples.put(DESCRIPTION_LABEL, description);
    }

    /**
     * @return the modelType
     */
    public Type getModelType() {
        Object type = tuples.get(MODEL_TYPE_LABEL);
        return type != null ? Type.findType(type.toString()) : null;
    }

    /**
     * @param type the modelType to set
     */
    public void setModelType(Type type) {
        tuples.put(MODEL_TYPE_LABEL, type);
    }

    /**
     * @return the visible
     */
    public boolean isVisible() {
        Object visible = tuples.get(VISIBLE_LABEL);
        return visible != null ? Boolean.parseBoolean(visible.toString()) : null;
    }

    /**
     * @param visible the visible to set
     */
    public void setVisible(boolean visible) {
        tuples.put(VISIBLE_LABEL, visible);
    }

    /**
     * @return the metadataType
     */
    public String getMetadataType() {
        Object type = tuples.get(METADATA_TYPE_LABEL);
        return type != null ? type.toString() : null;
    }

    /**
     * @param metadataType the metadataType to set
     */
    public void setMetadataType(String metadataType) {
        tuples.put(METADATA_TYPE_LABEL, metadataType);
    }

    /**
     * @return the ddl
     */
    public String getDdl() {
        Object ddl = tuples.get(JsonConstants.DDL_ATTRIBUTE);
        return ddl != null ? ddl.toString() : null;
    }

    /**
     * @param ddl the ddl to set
     */
    public void setDdl(String ddl) {
        tuples.put(JsonConstants.DDL_ATTRIBUTE, ddl);
    }
}
