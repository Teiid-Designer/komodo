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
package org.komodo.rest.relational.response;

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
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.vdb.lexicon.CoreLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * A VDB model that can be used by GSON to build a JSON document model representation.
 */
public final class RestVdbModel extends RestBasicEntity {

    /**
     * Label used to describe name
     */
    public static final String DESCRIPTION_LABEL = KomodoService.protectPrefix(VdbLexicon.Model.DESCRIPTION);

    /**
     * Label used to describe model type
     */
    public static final String MODEL_TYPE_LABEL = KomodoService.protectPrefix(CoreLexicon.JcrId.MODEL_TYPE);

    /**
     * Label used to describe visible
     */
    public static final String VISIBLE_LABEL = KomodoService.protectPrefix(VdbLexicon.Model.VISIBLE);

    /**
     * Label used to describe metadata type
     */
    public static final String METADATA_TYPE_LABEL = KomodoService.protectPrefix(VdbLexicon.Model.METADATA_TYPE);

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
        super(baseUri, model, uow, false);

        setDescription(model.getDescription(uow));
        setModelType(model.getModelType(uow));
        setVisible(model.isVisible(uow));
        setMetadataType(model.getMetadataType(uow));

        addExecutionProperties(uow, model);

        Properties properties = new Properties();
        byte[] ddlBytes = model.export(uow, properties);
        String ddl;
        if (ddlBytes == null)
            ddl = EMPTY_STRING;
        else
            ddl = new String(ddlBytes);

        setDdl(ddl);

        Vdb vdb = ancestor(model, Vdb.class, uow);
        ArgCheck.isNotNull(vdb);
        String vdbName = vdb.getName(uow);

        Properties settings = getUriBuilder().createSettings(SettingNames.VDB_NAME, vdbName);
        getUriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, getUriBuilder().vdbParentUri(vdb, uow));
        getUriBuilder().addSetting(settings, SettingNames.MODEL_NAME, getId());

        addLink(new RestLink(LinkType.SELF, getUriBuilder().vdbModelUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().vdbModelUri(LinkType.PARENT, settings)));
        createChildLink();
        addLink(new RestLink(LinkType.SOURCES, getUriBuilder().vdbModelUri(LinkType.SOURCES, settings)));
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
        return type != null ? Type.findType(type.toString()) : Type.DEFAULT_VALUE;
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
        return visible != null ? Boolean.parseBoolean(visible.toString()) : Model.DEFAULT_VISIBLE;
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
        return type != null ? type.toString() : Model.DEFAULT_METADATA_TYPE;
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
        return ddl != null ? ddl.toString() : EMPTY_STRING;
    }

    /**
     * @param ddl the ddl to set
     */
    public void setDdl(String ddl) {
        tuples.put(JsonConstants.DDL_ATTRIBUTE, ddl);
    }
}
