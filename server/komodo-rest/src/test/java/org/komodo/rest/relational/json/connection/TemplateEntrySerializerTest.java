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
package org.komodo.rest.relational.json.connection;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.relational.template.Template;
import org.komodo.relational.template.TemplateEntry;
import org.komodo.repository.RepositoryImpl;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.rest.relational.connection.RestTemplateEntry;
import org.komodo.rest.relational.json.AbstractSerializerTest;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.spi.repository.KomodoType;
import org.mockito.Mockito;

@SuppressWarnings( {"javadoc", "nls"} )
public final class TemplateEntrySerializerTest extends AbstractSerializerTest {

    private static final KomodoRestUriBuilder BUILDER = new KomodoRestUriBuilder(MY_BASE_URI);

    private static final String TEIID_DATA_PATH = RepositoryImpl.ENV_ROOT + FORWARD_SLASH +
        KomodoLexicon.TeiidCache.NODE_TYPE;

    private static final String TEMPLATE_NAME = "webservice";

    private static final String TEMPLATE_DATA_PATH = TEIID_DATA_PATH + FORWARD_SLASH +
        CachedTeiid.TEMPLATES_FOLDER + FORWARD_SLASH +
        TEMPLATE_NAME;

    private static final String TEMPLATE_URI = BUILDER.teiidCacheUri() + FORWARD_SLASH +
        V1Constants.TEMPLATES_SEGMENT + FORWARD_SLASH +
        TEMPLATE_NAME;

    private static final String ENTRY_NAME = "xa-resource-timeout";

    private static final String ENTRY_DATA_PATH = TEMPLATE_DATA_PATH + FORWARD_SLASH + 
        ENTRY_NAME;

    private static final String TEMPLATE_ENTRY_URI = TEMPLATE_URI + FORWARD_SLASH +
        V1Constants.TEMPLATE_ENTRIES_SEGMENT + FORWARD_SLASH +
        ENTRY_NAME;

    private static final int[] ALLOWED_VALUES = { 0, 1, 2 };

    private static final String SOME_CATEGORY = "SomeCategory";

    private static final String ENTRY_DESCRIPTION = "The entry description";

    private static final boolean ADVANCED_FLAG = true;

    private static final boolean MODIFIABLE_FLAG = true;

    private static final boolean REQUIRED_FLAG = true;

    private static final boolean CTA_FLAG = true;

    private static final boolean MASKED_FLAG = false;

    private static final String DISPLAY_NAME = "Entry Display Name";

    private static final String TYPE_CLASS_NAME = "java.lang.String";

    private static final String JSON = EMPTY_STRING +
        OPEN_BRACE + NEW_LINE +
            tab(1) + q("keng__baseUri") + colon() + q(MY_BASE_URI.toString()) + COMMA + NEW_LINE +
            tab(1) + q("keng__id") + colon() +  q(ENTRY_NAME) + COMMA + NEW_LINE +
            tab(1) + q("keng__dataPath") + colon() +  q(ENTRY_DATA_PATH) + COMMA + NEW_LINE +
            tab(1) + q("keng__kType") + colon() +  q(KomodoType.TEMPLATE_ENTRY.toString()) + COMMA + NEW_LINE +
            tab(1) + q("keng__hasChildren") + colon() +  Boolean.FALSE.toString() + COMMA + NEW_LINE +
            tab(1) + q(RestTemplateEntry.CATEGORY_LABEL) + colon() + q(SOME_CATEGORY) + COMMA + NEW_LINE +
            tab(1) + q(RestTemplateEntry.DEFAULT_VALUE_LABEL) + colon() + ALLOWED_VALUES[0] + COMMA + NEW_LINE +
            tab(1) + q(RestTemplateEntry.DESCRIPTION_LABEL) + colon() + q(ENTRY_DESCRIPTION) + COMMA + NEW_LINE +
            tab(1) + q(RestTemplateEntry.DISPLAY_NAME_LABEL) + colon() + q(DISPLAY_NAME) + COMMA + NEW_LINE +
            tab(1) + q(RestTemplateEntry.TYPE_CLASS_NAME_LABEL) + colon() + q(TYPE_CLASS_NAME) + COMMA + NEW_LINE +
            tab(1) + q(RestTemplateEntry.ADVANCED_LABEL) + colon() +  true + COMMA + NEW_LINE +
            tab(1) + q(RestTemplateEntry.CONSTRAINED_ALLOWED_VALUES_LABEL) + colon() +  true + COMMA + NEW_LINE +
            tab(1) + q(RestTemplateEntry.MASKED_LABEL) + colon() +  false + COMMA + NEW_LINE +
            tab(1) + q(RestTemplateEntry.MODIFIABLE_LABEL) + colon() +  true + COMMA + NEW_LINE +
            tab(1) + q(RestTemplateEntry.REQUIRED_LABEL) + colon() +  true + COMMA + NEW_LINE +
            tab(1) + q(RestTemplateEntry.ALLOWED_VALUES_LABEL) + colon() +  OPEN_SQUARE_BRACKET + NEW_LINE +
                tab(2) + ALLOWED_VALUES[0]+ COMMA + NEW_LINE +
                tab(2) + ALLOWED_VALUES[1] + COMMA + NEW_LINE +
                tab(2) + ALLOWED_VALUES[2] + NEW_LINE +
            tab(1) + CLOSE_SQUARE_BRACKET + COMMA + NEW_LINE +
            tab(1) + q(LINKS) + colon() + OPEN_SQUARE_BRACKET + NEW_LINE +
                tab(2) + OPEN_BRACE + NEW_LINE +
                    tab(3) + q("rel") + colon() + q("self") + COMMA + NEW_LINE +
                    tab(3) + q("href") + colon() + q(TEMPLATE_ENTRY_URI) + NEW_LINE +
                tab(2) + CLOSE_BRACE + COMMA + NEW_LINE +
                tab(2) + OPEN_BRACE + NEW_LINE +
                    tab(3) + q("rel") + colon() + q("parent") + COMMA + NEW_LINE +
                    tab(3) + q("href") + colon() + q(TEMPLATE_URI) + NEW_LINE +
                tab(2) + CLOSE_BRACE + NEW_LINE +
            tab(1) + CLOSE_SQUARE_BRACKET + NEW_LINE +
        CLOSE_BRACE;

    private RestTemplateEntry templateEntry;

    @Before
    public void init() throws Exception {
        CachedTeiid teiid = mockObject(CachedTeiid.class, "DefaultServer", TEIID_DATA_PATH, KomodoType.CACHED_TEIID, true, KomodoLexicon.CachedTeiid.NODE_TYPE);

        Template template = mockObject(Template.class, TEMPLATE_NAME, TEMPLATE_DATA_PATH, KomodoType.TEMPLATE, true, KomodoLexicon.Folder.NODE_TYPE);
        Mockito.when(template.getParent(transaction)).thenReturn(teiid);

        TemplateEntry entry = mockObject(TemplateEntry.class, ENTRY_NAME, ENTRY_DATA_PATH, KomodoType.TEMPLATE_ENTRY, false);
        Mockito.when(entry.getParent(transaction)).thenReturn(template);

        this.templateEntry = new RestTemplateEntry(MY_BASE_URI, entry, transaction);
        this.templateEntry.setAdvanced(ADVANCED_FLAG);
        this.templateEntry.setModifiable(MODIFIABLE_FLAG);
        this.templateEntry.setRequired(REQUIRED_FLAG);
        this.templateEntry.setConstrainedToAllowedValues(CTA_FLAG);
        this.templateEntry.setMasked(MASKED_FLAG);
        List<Integer> allowedValuesList = new ArrayList<Integer>();
        for (int val : ALLOWED_VALUES)
            allowedValuesList.add(val);

        this.templateEntry.setAllowedValues(allowedValuesList);
        this.templateEntry.setCategory(SOME_CATEGORY);
        this.templateEntry.setDefaultValue(ALLOWED_VALUES[0]);
        this.templateEntry.setDescription(ENTRY_DESCRIPTION);
        this.templateEntry.setDisplayName(DISPLAY_NAME);
        this.templateEntry.setTypeClassName(TYPE_CLASS_NAME);
    }

    @Test
    public void shouldExportJson() {
        String json = KomodoJsonMarshaller.marshall(this.templateEntry);
        assertEquals(JSON, json);
        System.out.println(JSON);
    }

    @Test
    public void shouldImportJson() {
        final RestTemplateEntry entry = KomodoJsonMarshaller.unmarshall(JSON, RestTemplateEntry.class);

        assertThat(entry.getId(), is(ENTRY_NAME));
        assertThat(entry.getAllowedValues(), is(ALLOWED_VALUES));
        assertThat(entry.getCategory(), is(SOME_CATEGORY));
        assertThat(entry.getDefaultValue(), is(ALLOWED_VALUES[0]));
        assertThat(entry.getDescription(), is(ENTRY_DESCRIPTION));
        assertThat(entry.getDisplayName(), is(DISPLAY_NAME));
        assertThat(entry.getTypeClassName(), is(TYPE_CLASS_NAME));
        assertThat(entry.isAdvanced(), is(ADVANCED_FLAG));
        assertThat(entry.isConstrainedToAllowedValues(), is(CTA_FLAG));
        assertThat(entry.isMasked(), is(MASKED_FLAG));
        assertThat(entry.isModifiable(), is(MODIFIABLE_FLAG));
        assertThat(entry.isRequired(), is(REQUIRED_FLAG));
    }
}
