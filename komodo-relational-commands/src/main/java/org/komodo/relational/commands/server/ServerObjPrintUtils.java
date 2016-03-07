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
package org.komodo.relational.commands.server;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.TreeMap;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.util.PrintUtils;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidPropertyDefinition;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.i18n.I18n;

/**
 *  Utilities for formatted printing for Server Objects
 */
public class ServerObjPrintUtils implements StringConstants {

    private static final int DEFAULT_WIDTH = 25;
    private static final int MAX_PROPERTY_VALUE_WIDTH = 100;  // Limit on the value column width

    /**
     * Print VDB Details
     * @param writer the Writer
     * @param indent number of indent spaces
     * @param vdb the vdb
     */
    public static void printVdbDetails(Writer writer, int indent, TeiidVdb vdb) {
        if(vdb!=null) {
            // ------------
            // VDB Name
            // ------------
            PrintUtils.print(writer, indent, "Name: " + vdb.getName()); //$NON-NLS-1$
            PrintUtils.print(writer, indent, "Version: " + vdb.getVersion()); //$NON-NLS-1$

            // ------------
            // VDB Type
            // ------------
            String vdbType = "Dynamic"; // Komodo only supports dynamic vdbs and ignores archive
            PrintUtils.print(writer, indent, "Type: "+vdbType); //$NON-NLS-1$

            // ------------
            // VDB Status
            // ------------
            String status = vdb.isActive() ? "ACTIVE" : "INACTIVE"; //$NON-NLS-1$ //$NON-NLS-2$
            PrintUtils.print(writer, indent, "Status: "+status); //$NON-NLS-1$

            // ------------
            // Models
            // ------------
            PrintUtils.print( writer, indent, StringConstants.EMPTY_STRING );
            Collection<String> modelNames = vdb.getModelNames();
            PrintUtils.printList(writer, new ArrayList< String >(modelNames), "VDB Models"); //$NON-NLS-1$

            // ------------
            // Properties
            // ------------
            PrintUtils.print( writer, indent, StringConstants.EMPTY_STRING );
            PrintUtils.print( writer, indent, "VDB Properties:" ); //$NON-NLS-1$
            Properties vdbProps = vdb.getProperties();
            PrintUtils.printProperties(writer, vdbProps, "Name", "Value"); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    /**
     * Print Translator Details
     * @param writer the Writer
     * @param indent number of indent spaces
     * @param translator the translator
     */
    public static void printTranslatorDetails(Writer writer, int indent, TeiidTranslator translator) {
        if(translator!=null) {
            // ------------
            // Translator Name
            // ------------
            PrintUtils.print(writer, indent, "Name: " + translator.getName()); //$NON-NLS-1$

            // ------------
            // Translator Type
            // ------------
            String transType = translator.getType();
            if(transType!=null) {
                PrintUtils.print(writer, indent, "Type: "+translator.getType()); //$NON-NLS-1$
            }

            // ------------
            // Properties
            // ------------
            PrintUtils.print( writer, indent, StringConstants.EMPTY_STRING );
            PrintUtils.print( writer, indent, "Translator Properties:" ); //$NON-NLS-1$
            Properties vdbProps = translator.getProperties();
            PrintUtils.printProperties(writer, vdbProps, "Name", "Value"); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    /**
     * Print Datasource Details
     * @param writer the Writer
     * @param indent number of indent spaces
     * @param source the datasource
     */
    public static void printDatasourceDetails(Writer writer, int indent, TeiidDataSource source) {
        if(source!=null) {
            // ------------
            // DataSource Name
            // ------------
            PrintUtils.print(writer, indent, "Name: " + source.getName()); //$NON-NLS-1$

            // ------------
            // DataSource Type
            // ------------
            PrintUtils.print(writer, indent, "Type: "+source.getType()); //$NON-NLS-1$

            // ------------
            // Properties
            // ------------
            PrintUtils.print( writer, indent, StringConstants.EMPTY_STRING );
            PrintUtils.print( writer, indent, "DataSource Properties:" ); //$NON-NLS-1$
            Properties vdbProps = source.getProperties();
            PrintUtils.printProperties(writer, vdbProps, "Name", "Value"); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }

    /**
     * Print Datasource type Details
     * @param writer the Writer
     * @param indent number of indent spaces
     * @param propDefns the property definitions
     * @param nameTitle the title of the name column
     * @param valueTitle the title of the value column
     */
    public static void printDatasourceTemplateProperties(Writer writer, int indent, Collection<TeiidPropertyDefinition> propDefns, String nameTitle, String valueTitle) {
        final Map< String, String > sorted = new TreeMap<>();
        int maxNameWidth = DEFAULT_WIDTH;
        int maxValueWidth = DEFAULT_WIDTH;

        for(TeiidPropertyDefinition propDefn : propDefns) {
            String name = propDefn.getDisplayName();
            Object value = propDefn.getDefaultValue();
            if(value==null) {
                value = I18n.bind( ShellI18n.noPropertyValue );
            }

            if ( maxNameWidth < name.length() ) {
                maxNameWidth = name.length();
            }

            if ( maxValueWidth < value.toString().length() ) {
                maxValueWidth = value.toString().length();
            }

            sorted.put( name, value.toString() );
        }

        // Puts a hard limit on value column width - some may be extremely long.  (The entire value will still be printed)
        if(maxValueWidth>MAX_PROPERTY_VALUE_WIDTH) {
            maxValueWidth = MAX_PROPERTY_VALUE_WIDTH;
        }

        final String format = PrintUtils.getFormat( maxNameWidth, maxValueWidth );

        PrintUtils.print( writer, MESSAGE_INDENT, String.format( format, nameTitle, valueTitle ) );
        PrintUtils.print( writer, MESSAGE_INDENT, String.format( format, PrintUtils.getHeaderDelimiter( maxNameWidth ), PrintUtils.getHeaderDelimiter( maxValueWidth ) ) );

        // print property name and value
        for ( final Entry< String, String > entry : sorted.entrySet() ) {
            String propName = entry.getKey();
            String propValue = entry.getValue();
            // propValue less than maximum width
            if(propValue.length() <= maxValueWidth) {
                PrintUtils.print( writer, MESSAGE_INDENT, String.format( format, propName, propValue ) );
                // propValue exceeds maximum width - splits it up onto separate lines
            } else {
                PrintUtils.printPropWithLongValue(writer,format,propName,propValue,null,maxValueWidth);
            }
        }
    }

}
