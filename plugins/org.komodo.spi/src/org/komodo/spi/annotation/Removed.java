/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.komodo.spi.annotation;

import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

import org.komodo.spi.runtime.version.TeiidVersion.Version;

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
