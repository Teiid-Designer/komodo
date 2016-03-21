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
package org.komodo.rest.json;

import org.komodo.spi.constants.StringConstants;

/**
 * Identifiers used in Komodo REST object JSON representations.
 */
public interface JsonConstants extends StringConstants {

    /**
     * Json-safe separator between prefix and attribute name
     */
    String PREFIX_SEPARATOR = UNDERSCORE + UNDERSCORE;

    /**
     * RegExp pattern for determining a property's prefix and name
     */
    String PREFIX_PATTERN = "([a-zA-Z]+):(.*)"; //$NON-NLS-1$

    /**
     * KEngine prefix
     */
    String KENGINE_PREFIX = "keng" + PREFIX_SEPARATOR; //$NON-NLS-1$

    /**
     * id property
     */
    String ID = KENGINE_PREFIX + "id"; //$NON-NLS-1$

    /**
     * id property
     */
    String BASE_URI = KENGINE_PREFIX + "baseUri"; //$NON-NLS-1$

    /**
     * path property
     */
    String DATA_PATH = KENGINE_PREFIX + "dataPath"; //$NON-NLS-1$

    /**
     * type property
     */
    String TYPE = KENGINE_PREFIX + "type"; //$NON-NLS-1$

    /**
     * kengine type property
     */
    String KTYPE = KENGINE_PREFIX + "kType"; //$NON-NLS-1$

    /**
     * has-children property
     */
    String HAS_CHILDREN = KENGINE_PREFIX + "hasChildren"; //$NON-NLS-1$

    /**
     * links property
     */
    String LINKS = KENGINE_PREFIX + "_links"; //$NON-NLS-1$

    /**
     * default property
     */
    String DEFAULT_VALUE = KENGINE_PREFIX + "defaultValue"; //$NON-NLS-1$

    /**
     * description property
     */
    String DESCRIPTION = KENGINE_PREFIX + "description"; //$NON-NLS-1$

    /**
     * properties attribute
     */
    String PROPERTIES = KENGINE_PREFIX + "properties"; //$NON-NLS-1$

    /**
     * Required property
     */
    String REQUIRED = KENGINE_PREFIX + "required"; //$NON-NLS-1$

    /**
     * Repeatable property
     */
    String REPEATABLE = KENGINE_PREFIX + "repeatable"; //$NON-NLS-1$

    /**
     * Limit property
     */
    String LIMIT = KENGINE_PREFIX + "limit"; //$NON-NLS-1$

    /**
     * Values property
     */
    String VALUES = KENGINE_PREFIX + "values"; //$NON-NLS-1$

    /**
     * Children property
     */
    String CHILDREN = KENGINE_PREFIX + "children"; //$NON-NLS-1$

    /**
     * DDL property
     */
    String DDL_ATTRIBUTE = KENGINE_PREFIX + "ddl"; //$NON-NLS-1$

    /**
     * relational property
     */
    String REL = "rel"; //$NON-NLS-1$

    /**
     * self link value
     */
    String SELF = "self"; //$NON-NLS-1$

    /**
     * href value
     */
    String HREF = "href"; //$NON-NLS-1$

    /**
     * parent link value
     */
    String PARENT = "parent"; //$NON-NLS-1$

    /**
     * null value
     */
    String NULL = "null"; //$NON-NLS-1$
}
