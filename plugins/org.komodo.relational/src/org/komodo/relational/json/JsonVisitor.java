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
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoObjectVisitor;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.UnitOfWork;
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

    private void property(String name, String value) {
        quoted(name);
        colon();
        quoted(value);
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

    private void properties(KomodoObject kObject, String label, KomodoType kType) throws Exception {
        String id = id(kObject);
        property(ID, id);
        comma();
        newline();

        property(NAME, transaction.decode(label));
        comma();
        newline();

        property(DATA_PATH, transaction.decode(kObject.getAbsolutePath()));
        comma();
        newline();

        property(TYPE, kType.getType());
        comma();
        newline();

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

    private void append(KomodoObject kObject, String name) throws Exception {
        KomodoType komodoType = kObject.getTypeIdentifier(transaction);
        properties(kObject, name, komodoType);
    }

    private void append(KomodoObject kObject) throws Exception {
        append(kObject, kObject.getName(transaction));
    }

    private void virtualDatabase(KomodoObject kObject) throws Exception {
        Property nameProperty = kObject.getProperty(transaction, VdbLexicon.Vdb.NAME);

        String vdbName = kObject.getName(transaction);
        if (nameProperty != null) {
            Object value = nameProperty.getValue(transaction);
            if (value != null)
                vdbName = value.toString();
        }

        append(kObject, vdbName);
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

        if(KomodoType.VDB.equals(type))
            virtualDatabase(kObject);
        else
            append(kObject);

        // decrement level
        lowerIndent();
        newline();
        closeBrace();

        return definition.toString();
    }
}
