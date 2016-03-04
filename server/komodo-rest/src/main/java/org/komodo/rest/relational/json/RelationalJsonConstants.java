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
package org.komodo.rest.relational.json;

import org.komodo.rest.json.JsonConstants;

/**
 * Identifiers used in Komodo REST relational object JSON representations.
 */
public interface RelationalJsonConstants extends JsonConstants {

    /**
     * Relational prefix
     */
    String RELATIONAL_PREFIX = "vdb" + PREFIX_SEPARATOR; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_CREATE_TEMP_TABLES = RELATIONAL_PREFIX + "allowCreateTempTables"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_ALTER = RELATIONAL_PREFIX + "allowAlter"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_CREATE = RELATIONAL_PREFIX + "allowCreate"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_DELETE = RELATIONAL_PREFIX + "allowDelete"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_EXECUTE = RELATIONAL_PREFIX + "allowExecute"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_LANGUAGE = RELATIONAL_PREFIX + "allowLanguage"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_READ = RELATIONAL_PREFIX + "allowRead"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ALLOW_UPDATE = RELATIONAL_PREFIX + "allowUpdate"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ANY_AUTHENTICATED = RELATIONAL_PREFIX + "anyAuthenticated"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String CONDITIONS = RELATIONAL_PREFIX + "conditions"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String NAME = RELATIONAL_PREFIX + "name"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String DESCRIPTION = RELATIONAL_PREFIX + "description"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String GRANT_ALL = RELATIONAL_PREFIX + "grantAll"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String IMPORT_DATA_POLICIES = RELATIONAL_PREFIX + "importDataPolicies"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String MAPPED_ROLES = RELATIONAL_PREFIX + "mappedRoles"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String MASKS = RELATIONAL_PREFIX + "masks"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String ORIGINAL_FILE = RELATIONAL_PREFIX + "originalFile"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String PATH = RELATIONAL_PREFIX + "path"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String PERMISSIONS = RELATIONAL_PREFIX + "permissions"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String TYPE = RELATIONAL_PREFIX + "type"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String VDBS = RELATIONAL_PREFIX + "vdbs"; //$NON-NLS-1$

    /**
     * Value is {@value}.
     */
    String VERSION = RELATIONAL_PREFIX + "version"; //$NON-NLS-1$

}
