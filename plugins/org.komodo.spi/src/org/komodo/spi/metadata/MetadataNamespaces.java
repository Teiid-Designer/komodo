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
package org.komodo.spi.metadata;

import org.komodo.spi.annotation.Since;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;

/**
 *
 */
public interface MetadataNamespaces {

    /**
     * Teiid Prefix
     */
    String TEIID_RESERVED = "teiid_"; //$NON-NLS-1$

    /**
     * Teiid Saleforce Prefix
     */
    String TEIID_SF = "teiid_sf"; //$NON-NLS-1$

    /**
     * Teiid Relational Prefix
     */
    String TEIID_RELATIONAL = "teiid_rel"; //$NON-NLS-1$

    /**
     * Teiid Web Services Prefix
     */
    String TEIID_WS = "teiid_ws"; //$NON-NLS-1$

    /**
     * Teiid Mongo Prefix
     */
    String TEIID_MONGO = "teiid_mongo"; //$NON-NLS-1$

    /**
     * Teiid Odata Prefix
     */
    String TEIID_ODATA = "teiid_odata"; //$NON-NLS-1$

    /**
     * Teiid Accumulo Prefix
     */
    @Since( Version.TEIID_8_7 )
    String TEIID_ACCUMULO = "teiid_accumulo"; //$NON-NLS-1$

    /**
     * Teiid Excel Prefix
     */
    @Since( Version.TEIID_8_7 )
    String TEIID_EXCEL = "teiid_excel"; //$NON-NLS-1$

    /**
     * Teiid JPA Prefix
     */
    @Since( Version.TEIID_8_7 )
    String TEIID_JPA = "teiid_jpa"; //$NON-NLS-1$

    /**
     * Relational URI
     */
    String RELATIONAL_URI = "{http://www.teiid.org/ext/relational/2012}"; //$NON-NLS-1$

    /**
     * Salesforce URI
     */
    String SF_URI = "{http://www.teiid.org/translator/salesforce/2012}"; //$NON-NLS-1$

    /**
     * Web Services URI
     */
    String WS_URI = "{http://www.teiid.org/translator/ws/2012}"; //$NON-NLS-1$

    /**
     * Mongo URI
     */
    String MONGO_URI = "{http://www.teiid.org/translator/mongodb/2013}"; //$NON-NLS-1$

    /**
     * Odata URI
     */
    String ODATA_URI = "{http://www.jboss.org/teiiddesigner/ext/odata/2012}"; //$NON-NLS-1$

    /**
     * Accumulo URI
     */
    @Since( Version.TEIID_8_7 )
    String ACCUMULO_URI = "{http://www.teiid.org/translator/accumulo/2013}"; //$NON-NLS-1$

    /**
     * Excel URI
     */
    @Since( Version.TEIID_8_7 )
    String EXCEL_URI = "{http://www.teiid.org/translator/excel/2014}"; //$NON-NLS-1$

    /**
     * JPA URI
     */
    @Since( Version.TEIID_8_7 )
    String JPA_URI = "{http://www.teiid.org/translator/jpa/2014}"; //$NON-NLS-1$

    /**
     * Enumerator of the namespaces
     */
    public enum URI {

        /**
         * Salesforce
         */
        SF(TEIID_SF, SF_URI),

        /**
         * Relational
         */
        RELATIONAL(TEIID_RELATIONAL, RELATIONAL_URI),

        /**
         * Web Services
         */
        WS(TEIID_WS, WS_URI),

        /**
         * Mongo
         */
        MONGO(TEIID_MONGO, MONGO_URI),

        /**
         * Odata
         */
        ODATA(TEIID_ODATA, ODATA_URI),

        /**
         * Accumulo
         */
        ACCUMULO(TEIID_ACCUMULO, ACCUMULO_URI),

        /**
         * Excel
         */
        EXCEL(TEIID_EXCEL, EXCEL_URI),

        /**
         * JPA
         */
        JPA(TEIID_JPA, JPA_URI);

        String prefix;

        String uri;

        String unbracedURI;

        private URI(String prefix, String uri) {
            this.prefix = prefix;
            this.uri = uri;
            this.unbracedURI = uri.replaceAll("{([\\S]+)}", "\\1"); //$NON-NLS-1$ //$NON-NLS-2$
        }

        /**
         * @return the prefix
         */
        public String getPrefix() {
            return this.prefix;
        }

        /**
         * @return the uri
         */
        public String getUri() {
            return this.uri;
        }

        /**
         * @return the unbracedURI
         */
        public String getUnbracedURI() {
            return this.unbracedURI;
        }
    }
}
