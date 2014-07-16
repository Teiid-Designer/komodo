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
package org.komodo.ddl.importer.node;

import org.komodo.ddl.importer.DdlNodeImporter;
import org.komodo.ddl.importer.ImportMessages;
import org.komodo.ddl.importer.ImportOptions;
import org.komodo.relational.model.RelationalObjectFactory;
import org.modeshape.sequencer.ddl.node.AstNode;

/**
 *  AbstractImporter - imports AstNodes
 */
public abstract class AbstractImporter implements DdlNodeImporter {

    /**
     * Entity Not Found Exception
     */
    protected class EntityNotFoundException extends Exception {

        private static final long serialVersionUID = 1L;

        /**
         * @param message the message text
         */
        public EntityNotFoundException(String message) {
            super(message);
        }
    }

    protected ImportOptions importOptions;
    protected ImportMessages importMessages;

    /**
     * @return the importOptions
     */
    protected ImportOptions getImportOptions() {
        return this.importOptions;
    }
    
    /**
     * @return the importMessages
     */
    protected ImportMessages getImportMessages() {
        return this.importMessages;
    }

    /**
    * @return relational factory
    */
    protected RelationalObjectFactory getFactory() {
        return RelationalObjectFactory.INSTANCE;
    }

    /**
     * @param message
     */
    protected void addProgressMessage(String message) {
        getImportMessages().addProgressMessage(message);
    }
    
    /**
     * Increments count of unhandled instances of a particular type
     * @param typeStr
     */
    protected void incrementUnhandledNodeType(String typeStr) {
    	getImportMessages().incrementUnhandledNodeType(typeStr);
    }

    /**
     * Gets boolean value for the provided text string
     *
     * @param text a text string
     * @return 'true' if provided string is "true", otherwise 'false'
     */
    protected boolean isTrue(String text) {
        return Boolean.valueOf(text);
    }

    /**
     * Is given node of the given dialect
     *
     * @param node
     * @param dialectType
     * @return true if node has dialect
     */
    protected boolean is(AstNode node, String dialectType) {
        return node.hasMixin(dialectType);
    }
}
