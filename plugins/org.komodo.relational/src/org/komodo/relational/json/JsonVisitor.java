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
package org.komodo.relational.json;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoObjectVisitor;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.jcr.ModeShapeLexicon;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * Converts a vdb into a JSON formatted object
 */
public class JsonVisitor implements JsonConstants, KomodoObjectVisitor, StringConstants {

    private UnitOfWork transaction;

    private StringBuffer definition = new StringBuffer();

    private String parentLink = EMPTY_STRING;

    private int level = 0;

    private List<KomodoType> filter = new ArrayList<KomodoType>();

    private boolean isFiltered(KomodoType type) {
        return filter.contains(type);
    }

    private static Pattern WHITE_SPACE_PATTERN = Pattern.compile("[\\s]+"); //$NON-NLS-1$

    private void removeTrailingComma() {
        int pos = definition.lastIndexOf(COMMA);
        if (pos == -1)
            return; // nothing to do

        if (pos == (definition.length() - 1)) {
            definition.deleteCharAt(pos);
            return;
        }

        String postComma = definition.substring(pos + 1);
        Matcher matcher = WHITE_SPACE_PATTERN.matcher(postComma);
        if (matcher.matches()) {
            definition.delete(pos, definition.length());
        }
    }

    private void closeSquareBracket() {
        definition.append(CLOSE_SQUARE_BRACKET);
    }

    private void openSquareBracket() {
        definition.append(OPEN_SQUARE_BRACKET);
    }

    private void closeBrace() {
        definition.append(CLOSE_BRACE);
    }

    private void openBrace() {
        definition.append(OPEN_BRACE);
    }

    private void quoted(String value) {
        if (value.contains(SPEECH_MARK)) {
            StringBuffer newValue = new StringBuffer();
            for (int i = 0; i < value.length(); ++i) {
                char v = value.charAt(i);
                if (v == '"')
                    newValue.append(DOUBLE_BACK_SLASH);

                newValue.append(v);
            }
            value = newValue.toString();
        }

        definition.append(SPEECH_MARK).append(value).append(SPEECH_MARK);
    }

    private void colon() {
        definition.append(SPACE).append(COLON).append(SPACE);
    }

    private void comma() {
        definition.append(COMMA);
    }

    private void raiseIndent() {
        level++;
    }

    private void lowerIndent() {
        level--;
    }

    private void newline() {
        definition.append(NEW_LINE);
        for (int i = 0; i < level; ++i)
            definition.append(TAB);
    }

    private void propertyValue(Object value) {
        if (value instanceof Integer || value instanceof Long ||
             value instanceof Double || value instanceof Float)
            definition.append(value);
        else
            quoted(value.toString());
    }

    private void property(String name, Object value) {
        quoted(name);
        colon();
        propertyValue(value);
    }

    private void property(String name, Object[] values) {
        if (values == null || values.length == 0)
            return;

        quoted(name);
        colon();
        openSquareBracket();
        for (int i = 0; i < values.length; ++i) {
            propertyValue(values[i]);
            if ((i + 1) < values.length)
                comma();
        }
        closeSquareBracket();
    }

    private String links(String id, String type) {
        boolean isRootNode = parentLink.isEmpty();

        if (isRootNode) {
            parentLink = FORWARD_SLASH + type.toLowerCase();
        }

        String selfLink = parentLink + FORWARD_SLASH + id;

        quoted(LINKS);
        colon();
        openSquareBracket();

        raiseIndent();
        newline();

        openBrace();
        raiseIndent();
        newline();

        property(REL, SELF);
        comma();
        newline();

        property(HREF, selfLink);
        lowerIndent();

        newline();
        closeBrace();
        // end of self link

        if (! isRootNode) {
            comma();
            newline();

            // start of parent link
            openBrace();
            raiseIndent();
            newline();

            property(REL, PARENT);
            comma();
            newline();

            property(HREF, parentLink);
            lowerIndent();

            newline();
            closeBrace();
        }

        lowerIndent();
        newline();
        closeSquareBracket();

        return selfLink;
    }

    private String id(KomodoObject kObject) throws KException {
        return transaction.decode(kObject.getName(transaction));
    }

    private boolean ignoreProperty(String propertyName) {
        if (propertyName.startsWith(JcrLexicon.Namespace.PREFIX))
            return true; // ignore built-in properties like primaryType and mixinTypes

        if (propertyName.startsWith(ModeShapeLexicon.Namespace.PREFIX))
            return true; // ignore built-in properties like mode:sha1

        if (propertyName.equals(VdbLexicon.Vdb.ORIGINAL_FILE))
            return true;

        if (propertyName.equals(VdbLexicon.Model.MODEL_DEFINITION))
            return true;

        if (propertyName.equals(StandardDdlLexicon.DDL_EXPRESSION))
            return true;

        if (propertyName.equals(StandardDdlLexicon.DDL_LENGTH))
            return true;

        if (propertyName.equals(StandardDdlLexicon.DDL_START_CHAR_INDEX))
            return true;

        if (propertyName.equals(StandardDdlLexicon.DDL_START_COLUMN_NUMBER))
            return true;

        if (propertyName.equals(StandardDdlLexicon.DDL_START_LINE_NUMBER))
            return true;

        if (propertyName.equals(TeiidDdlLexicon.CreateTable.QUERY_EXPRESSION))
            return true;

        if (propertyName.equals(TeiidDdlLexicon.CreateProcedure.STATEMENT))
            return true;

        return false;
    }

    private void properties(KomodoObject kObject, KomodoType kType) throws Exception {
        String id = id(kObject);
        property(ID, id);
        comma();
        newline();

        property(DATA_PATH, transaction.decode(kObject.getAbsolutePath()));
        comma();
        newline();

        property(TYPE, kType.getType());
        comma();
        newline();

        final List<String> propNames = new ArrayList<>(Arrays.asList(kObject.getPropertyNames(transaction))); // props with values
        final PropertyDescriptor[] descriptors = kObject.getPropertyDescriptors(transaction);

        if (descriptors.length != 0) {
            for (PropertyDescriptor descriptor : descriptors) {
                String name = descriptor.getName();
                if (!propNames.contains(name)) {
                    propNames.add(name);
                }
            }
        }

        Iterator<String> propIter = propNames.iterator();
        while(propIter.hasNext()) {
            String propName = propIter.next();
            Property property = kObject.getProperty(transaction, propName);
            if (property == null)
                continue;

            if (ignoreProperty(propName))
                continue;

            if (property.isMultiple(transaction))
                property(propName, property.getValues(transaction));
            else
                property(propName, property.getValue(transaction));

            if (propIter.hasNext()) {
                comma();
                newline();
            }
        }

        String selfLink = links(id, kType.getType());
        comma();
        newline();

        boolean hasChildren = kObject.hasChildren(transaction);
        property(HAS_CHILDREN, Boolean.toString(hasChildren));

        if (!hasChildren)
            return;

        comma();
        newline();

        /*
         * Prepare to navigate through the children
         */

        String currentParentLink = this.parentLink;
        this.parentLink = selfLink;

        KomodoObject[] children = kObject.getChildren(transaction);
        for (int i = 0; i < children.length; ++i) {
            KomodoObject child = children[i];
            if (isFiltered(child.getTypeIdentifier(transaction)))
                continue;

            /*
             * Create a nested object property, eg. "PARTS" : { "id" : PARTS, "type", "Model" }
             */
            quoted(id(child));
            colon();
            child.accept(transaction, this);

            if ((i + 1) < children.length) {
                comma();
                newline();
            }
        }

        // Possible due to filtering that a comma may be left appended
        // to the final child so just check and remove
        removeTrailingComma();

        // Restore the former parent link
        this.parentLink = currentParentLink;
    }

    private void append(KomodoObject kObject) throws Exception {
        KomodoType komodoType = kObject.getTypeIdentifier(transaction);
        properties(kObject, komodoType);
    }

    /**
     * Set types to exclude from the visitor.
     * This will exclude these types and their children.
     *
     * @param types types to exclude
     */
    public void setFilter(KomodoType... types) {
        if (types == null)
            return;

        for (KomodoType type : types) {
            filter.add(type);
        }
    }

    /**
     * @param kObject the object to be converted
     * @param transaction the transaction to use for this conversion
     * @throws Exception if error occurs
     */
    @Override
    public String visit(UnitOfWork transaction, KomodoObject kObject) throws Exception {
        this.transaction = transaction;
        if (kObject == null)
            return definition.toString();

        KomodoType type = kObject.getTypeIdentifier(transaction);
        if (isFiltered(type))
            return definition.toString();

        openBrace();

        // increment level
        raiseIndent();
        newline();

        append(kObject);

        // decrement level
        lowerIndent();
        newline();
        closeBrace();

        return definition.toString();
    }
}
