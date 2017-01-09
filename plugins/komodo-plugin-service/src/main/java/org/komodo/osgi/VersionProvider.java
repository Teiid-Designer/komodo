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
package org.komodo.osgi;

import java.util.MissingResourceException;
import java.util.ResourceBundle;
import org.komodo.utils.KLog;

/**
 * Reads the dependency versions properties file and provides the
 * versions of 3rd party libraries. OSGI wiring requires the version
 * pragma being appended to all 3rd party libraries so this provides
 * the values required.
 */
public final class VersionProvider {

    private static final String MODESHAPE_VERSION_PROPERTY = "modeshapeVersion";

    private static final String JAVAX_JCR_VERSION_PROPERTY = "javaxJcrVersion";

    private static final String JAVAX_XML_STREAM_VERSION_PROPERTY = "javaxXmlStream";

    private static final String JAVAX_TRANSACTION_VERSION_PROPERTY = "javaxTransaction";
    private static final String VERSIONS_PROPERTIES_FILENAME = "dependency-versions";

    private static final VersionProvider instance = new VersionProvider();

    public static VersionProvider getInstance() {
        return instance;
    }

    private String jcrVersion;

    private String modeshapeVersion;

    private String javaxXmlStreamVersion;

    private String javaxTransactionVersion;
    private VersionProvider() {
        ResourceBundle rb;
        try {
            rb = ResourceBundle.getBundle(VERSIONS_PROPERTIES_FILENAME);
            jcrVersion = rb.getString(JAVAX_JCR_VERSION_PROPERTY);
            modeshapeVersion = rb.getString(MODESHAPE_VERSION_PROPERTY);
            javaxXmlStreamVersion = rb.getString(JAVAX_XML_STREAM_VERSION_PROPERTY);
            javaxTransactionVersion= rb.getString(JAVAX_TRANSACTION_VERSION_PROPERTY);
        } catch (MissingResourceException e) {
            KLog.getLogger().error("Resource bundle '" + VERSIONS_PROPERTIES_FILENAME + "' was not found while reading version properties.");
        }
    }

    public String getJcrVersion() {
        return jcrVersion;
    }

    public String getModeshapeVersion() {
        return modeshapeVersion;
    }

    public String getJavaxXmlStreamVersion() {
        return javaxXmlStreamVersion;
    }

    public String getJavaxTransactionVersion() {
        return javaxTransactionVersion;
    }
}
