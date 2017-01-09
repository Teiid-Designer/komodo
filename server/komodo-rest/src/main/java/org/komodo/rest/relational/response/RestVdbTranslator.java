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
import org.komodo.relational.vdb.Translator;
import org.komodo.rest.KomodoService;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * A translator that can be used by GSON to build a JSON document representation.
 *
 * <pre>
 * <code>
 * {
 *     "id" : "MyTranslator",
 *     "description" : "translator description goes here",
 *     "type" : "customType",
 *     "properties" : [
 *         "green" : "lantern",
 *         "captain" : "america",
 *         "black" : "widow"
 *     ]
 * }
 * </code>
 * </pre>
 */
public final class RestVdbTranslator extends RestBasicEntity {

    /**
     * Label used to describe description
     */
    public static final String DESCRIPTION_LABEL = KomodoService.protectPrefix(VdbLexicon.Translator.DESCRIPTION);

    /**
     * Label used to describe type
     */
    public static final String TYPE_LABEL = KomodoService.protectPrefix(VdbLexicon.Translator.TYPE);

    /**
     * An empty array of translators.
     */
    public static final RestVdbTranslator[] NO_TRANSLATORS = new RestVdbTranslator[ 0 ];

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestVdbTranslator() {
        // nothing to do
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param translator the translator
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestVdbTranslator(URI baseUri, Translator translator, UnitOfWork uow) throws KException {
        super(baseUri, translator, uow, false);

        setDescription(translator.getDescription(uow));
        setType(translator.getType(uow));

        addExecutionProperties(uow, translator);

        Properties settings = getUriBuilder().createSettings(SettingNames.TRANSLATOR_NAME, getId());
        URI parentUri = getUriBuilder().vdbTranslatorParentUri(translator, uow);
        getUriBuilder().addSetting(settings, SettingNames.PARENT_PATH, parentUri);
        
        // VdbTranslators segment is added for Translators in a VDB
        KomodoObject parentObject = translator.getParent(uow);
        if(parentObject!=null && VdbLexicon.Vdb.VIRTUAL_DATABASE.equals(parentObject.getPrimaryType(uow).getName())) {
            getUriBuilder().addSetting(settings, SettingNames.ADD_TRANSLATORS_SEGMENT, "true"); //$NON-NLS-1$
        }

        addLink(new RestLink(LinkType.SELF, getUriBuilder().vdbTranslatorUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().vdbTranslatorUri(LinkType.PARENT, settings)));
        createChildLink();
    }

    /**
     * @return the description (can be empty)
     */
    public String getDescription() {
        Object description = tuples.get(DESCRIPTION_LABEL);
        return description != null ? description.toString() : null;
    }

    /**
     * @param newDescription
     *        the new description (can be empty)
     */
    public void setDescription( final String newDescription ) {
        tuples.put(DESCRIPTION_LABEL, newDescription);
    }

    /**
     * @return the translator type (can be empty)
     */
    public String getType() {
        Object type = tuples.get(TYPE_LABEL);
        return type != null ? type.toString() : null;
    }

    /**
     * @param newType
     *        the new translator type (can be empty)
     */
    public void setType( final String newType ) {
        tuples.put(TYPE_LABEL, newType);
    }
}
