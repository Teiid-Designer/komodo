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
package org.komodo.spi.query.sql.lang;

import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.query.sql.LanguageVisitor;

/**
 *
 */
public interface JoinType <LV extends LanguageVisitor> extends LanguageObject<LV>, StringConstants {

    /**
     * Used only for comparison during equals, not by users of this class
     * 
     * @return Type code for object
     */
    int getTypeCode();

    /**
     * Check if this join type is an outer join.
     * 
     * @return True if left/right/full outer, false if inner/cross
     */
    boolean isOuter();
}
