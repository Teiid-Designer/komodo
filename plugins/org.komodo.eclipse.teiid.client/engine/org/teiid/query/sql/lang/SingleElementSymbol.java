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
package org.teiid.query.sql.lang;

import org.komodo.spi.annotation.Removed;
import org.komodo.spi.runtime.version.TeiidServerVersion.Version;
import org.teiid.query.sql.symbol.Expression;

/**
 * Interface only applicable to the Teiid 7 parser
 */
@Removed(Version.TEIID_8_0)
public interface SingleElementSymbol extends Expression {

}
