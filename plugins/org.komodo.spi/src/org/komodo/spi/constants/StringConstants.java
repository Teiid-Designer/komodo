package org.komodo.spi.constants;
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


/**
 * 
 *
 *
 */
public interface StringConstants {

    /**
     * An empty string
     */
    String EMPTY_STRING = ""; //$NON-NLS-1$

    /**
     * A space.
     */
    String SPACE = " "; //$NON-NLS-1$

    /**
     * A star.
     */
    String STAR = "*"; //$NON-NLS-1$

    /**
     * An underscore.
     */
    String UNDERSCORE = "_"; //$NON-NLS-1$

    /**
     * The String "\n"
     */
    String NEW_LINE = "\n"; //$NON-NLS-1$
    
    /**
     * A Comma.
     */
    String COMMA = ","; //$NON-NLS-1$

    /**
     * A Dot.
     */
    String DOT = "."; //$NON-NLS-1$

    /**
     * class extension
     */
    String CLASS = "class"; //$NON-NLS-1$

    /**
     * xml extension
     */
    String XML = "xml"; //$NON-NLS-1$

    /**
     * The name of the System property that specifies the string that should be used to separate lines. This property is a standard
     * environment property that is usually set automatically.
     */
    String LINE_SEPARATOR_PROPERTY_NAME = "line.separator"; //$NON-NLS-1$

    /**
     * The String that should be used to separate lines; defaults to {@link #NEW_LINE}
     */
    String LINE_SEPARATOR = System.getProperty(LINE_SEPARATOR_PROPERTY_NAME, NEW_LINE);

    /**
     * Forward slash
     */
    String FORWARD_SLASH = "/"; //$NON-NLS-1$

    /**
     * Back slash used in regular expressions
     */
    String DOUBLE_BACK_SLASH = "\\"; //$NON-NLS-1$

    /**
     * Equals
     */
    String EQUALS = "="; //$NON-NLS-1$

    /**
     * Open Angle Bracket
     */
	String OPEN_ANGLE_BRACKET = "<"; //$NON-NLS-1$

	/**
	 * Close Angle Bracket
	 */
	String CLOSE_ANGLE_BRACKET = ">"; //$NON-NLS-1$

	/**
     * komodo
     */
    String KOMODO = "komodo"; //$NON-NLS-1$

    /**
     * .komodo
     */
    String DOT_KOMODO = ".komodo"; //$NON-NLS-1$

    /**
     * log
     */
    String LOG = "log"; //$NON-NLS-1$


}
