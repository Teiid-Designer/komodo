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
package org.komodo.repository;

import java.io.File;
import java.io.PrintStream;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoObjectVisitor;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.ArgCheck;
import org.modeshape.jcr.api.JcrConstants;
import org.modeshape.jcr.api.JcrTools;

/**
 * Utility functions for adding remove objects from repositories
 */
public class RepositoryTools implements StringConstants {

    private RepositoryTools() {}

    /**
     * Get or create a KomodoObject at the specified path.
     * @param transaction transaction
     * @param parent the parent KomodoObject. may not be null
     * @param path the path of the desired child KomodoObject. may not be null
     * @param defaultType the default KomodoObject type. may be null
     * @param finalType the optional final KomodoObject type. may be null
     * @return the existing or newly created KomodoObject
     * @throws Exception if an error occurs
     * @throws IllegalArgumentException if either the parent or path argument is null
     */
    public static KomodoObject findOrCreate(UnitOfWork transaction,
                                  KomodoObject parent,
                                  String path,
                                  String defaultType,
                                  String finalType ) throws Exception {
        ArgCheck.isNotNull(parent, "parentKomodoObject"); //$NON-NLS-1$
        ArgCheck.isNotNull(path, "path"); //$NON-NLS-1$
        // Remove leading and trailing slashes ...
        String relPath = path.replaceAll("^/+", "").replaceAll("/+$", ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

        // Look for the KomodoObject first ...
        if (parent.hasChild(transaction, relPath))
            return parent.getChild(transaction, relPath);

        // Create the KomodoObject, which has to be done segment by segment ...
        String[] pathSegments = relPath.split("/"); //$NON-NLS-1$
        KomodoObject komodoObject = parent;
        for (int i = 0, len = pathSegments.length; i != len; ++i) {
            String pathSegment = pathSegments[i];
            pathSegment = pathSegment.trim();
            if (pathSegment.length() == 0) continue;
            if (komodoObject.hasChild(transaction, pathSegment)) {
                // Find the existing KomodoObject ...
                komodoObject = komodoObject.getChild(transaction, pathSegment);
            } else {
                // Make sure there is no index on the final segment ...
                String pathSegmentWithNoIndex = pathSegment.replaceAll("(\\[\\d+\\])+$", ""); //$NON-NLS-1$ //$NON-NLS-2$
                // Create the KomodoObject ...
                String komodoObjectType = defaultType;
                if (i == len - 1 && finalType != null)
                    komodoObjectType = finalType;

                if (komodoObjectType != null) {
                    komodoObject = komodoObject.addChild(transaction, pathSegmentWithNoIndex, komodoObjectType);
                } else {
                    komodoObject = komodoObject.addChild(transaction, pathSegmentWithNoIndex, JcrConstants.NT_UNSTRUCTURED);
                }
            }
        }

        return komodoObject;
    }

    /**
     * Get or create a KomodoObject with the specified KomodoObject under the specified parent KomodoObject.
     * @param transaction the transaction
     * @param parent the parent KomodoObject. may not be null
     * @param name the name of the child KomodoObject. may not be null
     * @return the existing or newly created child KomodoObject
     * @throws Exception if an error occurs
     * @throws IllegalArgumentException if either the parent or name argument is null
     */
    public static KomodoObject findOrCreateChild(UnitOfWork transaction, KomodoObject parent, String name ) throws Exception {
        return findOrCreateChild(transaction, parent, name, null);
    }

    /**
     * Get or create a KomodoObject with the specified KomodoObject and KomodoObject type under the specified parent KomodoObject.
     * @param transaction the transaction
     * @param parent the parent KomodoObject. may not be null
     * @param name the name of the child KomodoObject. may not be null
     * @param komodoObjectType the KomodoObject type. may be null
     * @return the existing or newly created child KomodoObject
     * @throws Exception if an error occurs
     */
    public static KomodoObject findOrCreateChild(UnitOfWork transaction,
                                   KomodoObject parent,
                                   String name,
                                   String komodoObjectType ) throws Exception {
        return findOrCreate(transaction, parent, name, komodoObjectType, komodoObjectType);
    }

    /**
     * @param property
     *        the property whose display value is being requested (cannot be empty)
     * @return String representation of property and its values
     */
    public static String getDisplayValue(Property property) {
        StringBuilder sb = new StringBuilder();
        try {
            sb.append(property.getName(null)).append('=');
            if (property.isMultiple(null)) {
                sb.append('[');
                Object[] values = property.getValues(null);
                for (int i = 0; i < values.length; ++i) {
                    Object value = values[i];
                    sb.append(value);
                    if ((i + 1) < values.length)
                        sb.append(',');
                }
                sb.append(']');
            } else {
                Object value = property.getValue(null);
                sb.append(value);
            }
        } catch (Exception e) {
            sb.append(" on deleted node ").append(property.getAbsolutePath()); //$NON-NLS-1$
        }

        return sb.toString();
    }

    /**
     * Traverses the graph of the given {@link KomodoObject} and returns its
     * String representation.
     *
     * @param kObject object to be traversed
     * @return string representation of object's graph
     * @throws Exception if error occurs
     */
    public static String traverse(KomodoObject kObject) throws Exception {
        TraversalOutputVisitor visitor = new TraversalOutputVisitor();
        return visitor.visit(kObject);
    }

    /**
     * Visitor for printing out the full tree of a {@link KomodoObject}.
     *
     * This can do the same job as {@link JcrTools#printNode(javax.jcr.Node)}, however
     * the latter only prints to System.out while this visitor returns the string allowing
     * it to be sent to alternative {@link PrintStream}s.
     *
     */
    public static class TraversalOutputVisitor implements KomodoObjectVisitor {

        private final StringBuffer buffer = new StringBuffer(NEW_LINE);

        private String createIndent(String path) {
            StringBuffer indent = new StringBuffer(TAB);
            String[] levels = path.split(File.separator);

            for (int i = 0; i < levels.length; ++i) {
                indent.append(TAB);
            }

            return indent.toString();
        }

        @Override
        public String visit(KomodoObject object) throws Exception {
            String indent = createIndent(object.getAbsolutePath());
            buffer.append(indent + object.getName(null) + NEW_LINE);

            String[] propertyNames = object.getPropertyNames(null);

            for (String propertyName : propertyNames) {
                Property property = object.getProperty(null, propertyName);
                buffer.append(indent + TAB + AT + getDisplayValue(property) + NEW_LINE);
            }

            KomodoObject[] children = object.getChildren(null);
            for (int i = 0; i < children.length; ++i)
                children[i].visit(this);

            return buffer.toString();
        }
    }
}
