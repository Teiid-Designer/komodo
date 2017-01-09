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
import org.komodo.relational.vdb.ModelSource;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * A VDB model source that can be used by GSON to build a JSON model source representation.
 */
public final class RestVdbModelSource extends RestBasicEntity {

    /**
     * Label used to describe jndi name
     */
    public static final String JNDI_NAME_LABEL = KomodoService.protectPrefix(VdbLexicon.Source.JNDI_NAME);

    /**
     * Label used to describe translator
     */
    public static final String TRANSLATOR_LABEL = KomodoService.protectPrefix(VdbLexicon.Source.TRANSLATOR);

    /**
     * Constructor for use when deserializing
     */
    public RestVdbModelSource() {
        super();
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param source the source
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestVdbModelSource(URI baseUri, ModelSource source, UnitOfWork uow) throws KException {
        super(baseUri, source, uow, false);

        setJndiName(source.getJndiName(uow));
        String translatorName = source.getTranslatorName(uow);
        setTranslator(translatorName);

        Model model = ancestor(source, Model.class, uow);
        ArgCheck.isNotNull(model);
        String modelName = model.getName(uow);

        Vdb vdb = ancestor(model, Vdb.class, uow);
        ArgCheck.isNotNull(vdb);
        String vdbName = vdb.getName(uow);

        Properties settings = getUriBuilder().createSettings(SettingNames.VDB_NAME, vdbName);
        getUriBuilder().addSetting(settings, SettingNames.VDB_PARENT_PATH, getUriBuilder().vdbParentUri(vdb, uow));
        getUriBuilder().addSetting(settings, SettingNames.MODEL_NAME, modelName);
        getUriBuilder().addSetting(settings, SettingNames.TRANSLATOR_NAME, translatorName);
        getUriBuilder().addSetting(settings, SettingNames.SOURCE_NAME, getId());

        addLink(new RestLink(LinkType.SELF, getUriBuilder().vdbModelSourceUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().vdbModelSourceUri(LinkType.PARENT, settings)));
        createChildLink();

        Translator[] translators = vdb.getTranslators(uow, translatorName);
        if (translators != null && translators.length == 1) {
            getUriBuilder().addSetting(settings, SettingNames.TRANSLATOR_NAME, translatorName);
            addLink(new RestLink(LinkType.REFERENCE, getUriBuilder().vdbModelSourceUri(LinkType.REFERENCE, settings)));
        }
    }

    /**
     * @return the jndiName
     */
    public String getJndiName() {
        Object jndi = tuples.get(JNDI_NAME_LABEL);
        return jndi != null ? jndi.toString() : null;
    }

    /**
     * @param jndiName the jndiName to set
     */
    public void setJndiName(String jndiName) {
        tuples.put(JNDI_NAME_LABEL, jndiName);
    }

    /**
     * @return the translator
     */
    public String getTranslator() {
        Object translator = tuples.get(TRANSLATOR_LABEL);
        return translator != null ? translator.toString() : null;
    }

    /**
     * @param translator the translator to set
     */
    public void setTranslator(String translator) {
        tuples.put(TRANSLATOR_LABEL, translator);
    }
}
