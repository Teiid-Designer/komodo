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
package org.komodo.rest.relational.connection;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import org.komodo.relational.template.Template;
import org.komodo.relational.template.TemplateEntry;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A template that can be used by GSON to build a JSON document representation.
 */
public final class RestTemplate extends RestBasicEntity {

    /**
     * Is-Jdbc label
     */
    public static final String IS_JDBC_LABEL = "isJdbc";

    /**
     * Label used to describe entries
     */
    public static final String ENTRIES_LABEL = "entries";

    /**
     * An empty array of templates.
     */
    public static final RestTemplate[] NO_TEMPLATES = new RestTemplate[0];

    private List<String> entries = Collections.emptyList();

    /**
     * Constructor for use <strong>only</strong> when deserializing.
     */
    public RestTemplate() {
        // nothing to do
    }

    /**
     * Constructor for use when serializing.
     * @param baseUri the base uri of the REST request
     * @param template the template
     * @param uow the transaction
     * @throws KException if error occurs
     */
    public RestTemplate(URI baseUri, Template template, UnitOfWork uow) throws KException {
        super(baseUri, template, uow, false);

        Properties settings = getUriBuilder().createSettings(SettingNames.TEMPLATE_NAME, getId());
        URI parentUri = getUriBuilder().templateParentUri(template, uow);
        getUriBuilder().addSetting(settings, SettingNames.PARENT_PATH, parentUri);

        setJdbc(template.isJdbc(uow));

        List<TemplateEntry> templateEntries = template.getEntries(uow);
        if (templateEntries != null) {
            entries = new ArrayList<String>();
            for (TemplateEntry entry : templateEntries) {
                entries.add(entry.getName(uow));
            }
        }

        addLink(new RestLink(LinkType.SELF, getUriBuilder().templateUri(LinkType.SELF, settings)));
        addLink(new RestLink(LinkType.PARENT, getUriBuilder().templateUri(LinkType.PARENT, settings)));
        createChildLink();
        addLink(new RestLink(LinkType.TEMPLATE_ENTRIES, getUriBuilder().templateUri(LinkType.TEMPLATE_ENTRIES, settings)));
    }

    /**
     * @return jdbc flag
     */
    public boolean isJdbc() {
        Object value = tuples.get(IS_JDBC_LABEL);
        return value != null ? Boolean.parseBoolean(value.toString()) : false;
    }

    /**
     * @param jdbc
     */
    public void setJdbc(boolean jdbc) {
        tuples.put(IS_JDBC_LABEL, jdbc);
    }

    public List<String> getEntries() {
        return Collections.unmodifiableList(entries);
    }

    public void setEntries(String[] entriesNames) {
        if (entriesNames == null)
            this.entries = Collections.emptyList();
        else
            this.entries = Arrays.asList(entriesNames);
    }

}
