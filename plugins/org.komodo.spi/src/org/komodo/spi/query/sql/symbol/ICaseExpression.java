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
package org.komodo.spi.query.sql.symbol;

import org.komodo.spi.query.sql.ILanguageVisitor;
import org.komodo.spi.query.sql.lang.IExpression;

/**
 *
 */
public interface ICaseExpression<LV extends ILanguageVisitor> extends IExpression<LV> {

    IExpression getExpression();

    int getWhenCount();

    IExpression getWhenExpression(int index);

    IExpression getThenExpression(int index);

    IExpression getElseExpression();
}
