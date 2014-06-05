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
 * Used for decorating classes, fields and methods defining
 * updated values for teiid versions.
 *
 * The fields act as pairs where with each new version the
 * value in 'replaced' was replaced. 
 *  
 */
@Retention(value = RetentionPolicy.RUNTIME)
@Inherited
public @interface Updated {

    /**
     * @return list of versions where item was updated
     */
    Version[] version();

    /**
     * @return list of values replaced when item was updated
     */
    String[] replaces();

}
