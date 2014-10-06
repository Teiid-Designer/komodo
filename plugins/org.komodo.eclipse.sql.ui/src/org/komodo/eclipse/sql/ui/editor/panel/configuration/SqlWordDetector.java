/*
 * "The Java Developer's Guide to Eclipse"
 *   by Shavor, D'Anjou, Fairbrother, Kehn, Kellerman, McCarthy
 * 
 * (C) Copyright International Business Machines Corporation, 2003. 
 * All Rights Reserved.
 * 
 * Code or samples provided herein are provided without warranty of any kind.
 */
package org.komodo.eclipse.sql.ui.editor.panel.configuration;

import org.eclipse.jface.text.rules.IWordDetector;
import org.komodo.spi.runtime.version.ITeiidVersion;

/**
 * Determines whether a given character is valid as part of an SQL keyword in 
 * the current context.
 *
 * @since 8.0
 */
public class SqlWordDetector implements IWordDetector {
   
    private SqlSyntax sqlSyntax;

    public SqlWordDetector(ITeiidVersion teiidVersion) {
        sqlSyntax = new SqlSyntax(teiidVersion);
    }

	/**
	 * @see org.eclipse.jface.text.rules.IWordDetector#isWordStart(char)
	 */
	@Override
	public boolean isWordStart(char c) {
        return sqlSyntax.isSqlWordStart(c);
	}

	/**
	 * @see org.eclipse.jface.text.rules.IWordDetector#isWordPart(char)
	 */
	@Override
	public boolean isWordPart(char c) {
        return sqlSyntax.isSqlWordPart(c);
	}

}
