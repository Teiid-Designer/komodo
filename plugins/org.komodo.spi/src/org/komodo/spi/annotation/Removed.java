/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 ************************************************************************************/
package org.komodo.spi.annotation;

import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;

/**
 * Used for decorating classes, fields and methods defining their
 * removal at a specific teiid teiid instance version
 */
@Retention(value = RetentionPolicy.RUNTIME)
@Inherited
public @interface Removed {

    /**
     * The version of the Teiid Instance that the class,
     * field or method is no longer applicable for, ie. has been removed.
     * The value is one of the values of the enum {@link Version}.
     *
     * The default minimum version is considered
     * {@link Version#TEIID_8_0}.
     */
    Version value() default Version.TEIID_8_0;
}
