package org.komodo.spi.constants;
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
     * The String "\t"
     */
    String TAB = "\n"; //$NON-NLS-1$
    
    /**
     * A Comma.
     */
    String COMMA = ","; //$NON-NLS-1$

    /**
     * A Dot.
     */
    String DOT = "."; //$NON-NLS-1$

    /**
     * Two Dots
     */
    String DOT_DOT = ".."; //$NON-NLS-1$

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

	/**
	 * Current Folder Symbol
	 */
	String CURRENT_FOLDER_SYMBOL = "."; //$NON-NLS-1$

	/**
	 * Parent Folder Symbol
	 */
	String PARENT_FOLDER_SYMBOL = ".."; //$NON-NLS-1$
	
	/**
	 * Drive Separator
	 */
	String DRIVE_SEPARATOR = ":"; //$NON-NLS-1$
	
	/**
	 * File Extension separator
	 */
	String FILE_EXTENSION_SEPARATOR = "."; //$NON-NLS-1$
	
	/**
	 * File name wildcard
	 */
	String FILE_NAME_WILDCARD = "*"; //$NON-NLS-1$

  /**
   * Target directory
   */
  String TARGET = "target"; //$NON-NLS-1$

  /**
   * sources jar component
   */
  String SOURCES = "sources"; //$NON-NLS-1$

  /**
   * JAR File Extension
   */
  String JAR = "jar"; //$NON-NLS-1$
}

