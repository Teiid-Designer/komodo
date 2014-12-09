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
package org.komodo.modeshape.visitor;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.jcr.Node;
import javax.jcr.Property;
import javax.jcr.PropertyIterator;
import javax.jcr.RepositoryException;
import javax.jcr.Value;
import javax.jcr.nodetype.NodeType;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import org.komodo.modeshape.AbstractNodeVisitor;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.sequencer.teiid.lexicon.CoreLexicon;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * Visitor that will walk a vdb node tree and convert it to
 * the dynamic vdb xml syntax.
 */
public class VdbNodeVisitor extends AbstractNodeVisitor implements StringConstants {

    /**
     * Companion XML tag for permission condition
     */
    public static String DATA_ROLE_PERMISSION_CONDITION_XML = "condition"; //$NON-NLS-1$

    private static final String UNDEFINED = "undefined"; //$NON-NLS-1$

    private static Map<String, NodeTypeName> nodeNameIndex = new HashMap<String, NodeTypeName>();

    private enum NodeTypeName {

        VIRTUAL_DATABASE(VdbLexicon.Vdb.VIRTUAL_DATABASE, VdbLexicon.ManifestIds.VDB),

        DESCRIPTION(VdbLexicon.Vdb.DESCRIPTION, VdbLexicon.ManifestIds.DESCRIPTION),

        IMPORT_VDBS(VdbLexicon.Vdb.IMPORT_VDBS),

        IMPORT_VDB(VdbLexicon.ImportVdb.IMPORT_VDB, VdbLexicon.ManifestIds.IMPORT_VDB),

        MODEL(VdbLexicon.Vdb.DECLARATIVE_MODEL, VdbLexicon.ManifestIds.MODEL),

        SOURCES(VdbLexicon.Vdb.SOURCES),

        SOURCE(VdbLexicon.Source.SOURCE, VdbLexicon.ManifestIds.SOURCE),

        TRANSLATORS(VdbLexicon.Vdb.TRANSLATORS),

        TRANSLATOR(VdbLexicon.Translator.TRANSLATOR, VdbLexicon.ManifestIds.TRANSLATOR),

        DATA_ROLES(VdbLexicon.Vdb.DATA_ROLES),

        DATA_ROLE(VdbLexicon.DataRole.DATA_ROLE, VdbLexicon.ManifestIds.DATA_ROLE),

        PERMISSIONS(VdbLexicon.DataRole.PERMISSIONS),

        PERMISSION(VdbLexicon.DataRole.Permission.PERMISSION, VdbLexicon.ManifestIds.PERMISSION),

        CONDITIONS(VdbLexicon.DataRole.Permission.CONDITIONS),

        CONDITION(VdbLexicon.DataRole.Permission.Condition.CONDITION, VdbLexicon.ManifestIds.CONDITION),

        MASKS(VdbLexicon.DataRole.Permission.MASKS),

        MASK(VdbLexicon.DataRole.Permission.Mask.MASK, VdbLexicon.ManifestIds.MASK),

        UNKNOWN(UNDEFINED);

        private final String id;

        private final String tag;

        private NodeTypeName(String id, String tag) {
            this.id = id;
            this.tag = tag;
            nodeNameIndex.put(this.id, this);
        }

        private NodeTypeName(String name) {
            this(name, UNDEFINED);
        }

        /**
         * @return the name
         */
        public String getId() {
            return this.id;
        }

        /**
         * @return the tag
         */
        public String getTag() {
            return this.tag;
        }

        /**
         * @param name name value
         * @return the name enum for the given name
         */
        public static NodeTypeName findName(String name) {
            NodeTypeName ntName = nodeNameIndex.get(name);
            if (ntName != null)
                return ntName;

            return UNKNOWN;
        }
    }

    private final XMLStreamWriter writer;

    /**
     * Create new visitor that writes to the given xml stream writer
     *
     * @param version teiid version
     * @param writer output for the xml
     */
    public VdbNodeVisitor(TeiidVersion version, XMLStreamWriter writer) {
        super(version);
        this.writer = writer;
    }

    @Override
    protected String undefined() {
        return UNDEFINED;
    }

    private void writeNewLine(int total) throws XMLStreamException {
        for (int i = 0; i < total; ++i)
            writer.writeCharacters(NEW_LINE);
    }

    private void writeNewLine() throws XMLStreamException {
        writeNewLine(1);
    }

    private void writeStartDocument() throws XMLStreamException {
        writer.writeStartDocument("UTF-8", "1.0"); //$NON-NLS-1$ //$NON-NLS-2$
        writeNewLine();
    }

    private void writeStartElement(String tag) throws XMLStreamException {
        writer.writeStartElement(tag);
    }

    private void writeAttribute(String name, String value) throws XMLStreamException {
        writer.writeAttribute(name, value);
    }

    private void writeCData(String data) throws XMLStreamException {
        writer.writeCData(data);
    }

    private void writeCharacters(String characters) throws XMLStreamException {
        writer.writeCharacters(characters);
    }

    private void writeEndElement() throws XMLStreamException {
        writer.writeEndElement();
        writeNewLine();
    }

    private void writeElementWithText(String name, String text) throws XMLStreamException {
        writeStartElement(name);
        writeCharacters(text);
        writeEndElement();
    }

    private void writeEndDocument() throws XMLStreamException {
        writer.writeEndDocument();
        writer.close();
    }

    private boolean isPrimaryNodeType(Node node, NodeTypeName nodeTypeName) throws RepositoryException {
        NodeType nodeType = node.getPrimaryNodeType();
        return nodeTypeName.getId().equals(nodeType.getName());
    }

    private void properties(Node node, String... propertiesToIgnore) throws XMLStreamException, RepositoryException {
        List<String> propsToIgnore = Arrays.asList(propertiesToIgnore);

        PropertyIterator propIter = node.getProperties();
        while(propIter.hasNext()) {
            Property property = propIter.nextProperty();
            String name = property.getName();
            if (name == null)
                continue;

            if (propsToIgnore.contains(name))
                continue;

            // Ignore jcr properties since these are internal to modeshape
            if (name.startsWith(JcrLexicon.Namespace.PREFIX))
                continue;

            String value = toString(property);

            //
            // Preview is actually converted into a vdb property so need to special-case
            // turn it back into a simple property name but we only care if the property
            // is actually true.
            //
            if (name.equals(VdbLexicon.Vdb.PREVIEW) && Boolean.parseBoolean(value))
                name = VdbLexicon.ManifestIds.PREVIEW;

            //
            // Ignore modeshape vdb properties as <property> type properties will
            // not have a vdb prefix but simply be the property name on its own, eg.
            // UseConnectedMetadata or vdb-property1.
            //
            if (name.startsWith(VdbLexicon.Namespace.PREFIX + COLON))
                continue;

            writeStartElement(VdbLexicon.ManifestIds.PROPERTY);
            writeAttribute(VdbLexicon.ManifestIds.NAME, name);
            writeAttribute(VdbLexicon.ManifestIds.VALUE, value);
            writeEndElement();
        }
    }

    private void mask(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.MASK))
            return;

        // Condition element
        writeStartElement(NodeTypeName.MASK.getTag());

        Property property = property(node, VdbLexicon.DataRole.Permission.Mask.ORDER);
        writeAttribute(VdbLexicon.ManifestIds.ORDER, toString(property));

        writeCharacters(node.getName());
        writeEndElement();
    }

    private void condition(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.CONDITION))
            return;

        // Condition element
        writeStartElement(NodeTypeName.CONDITION.getTag());

        Property property = property(node, VdbLexicon.DataRole.Permission.Condition.CONSTRAINT);
        writeAttribute(VdbLexicon.ManifestIds.CONSTRAINT, toString(property));

        writeCharacters(node.getName());
        writeEndElement();
    }

    private void permission(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.PERMISSION))
            return;

        // Permission element
        writeStartElement(NodeTypeName.PERMISSION.getTag());

        // Resource name element
        writeElementWithText(VdbLexicon.ManifestIds.RESOURCE_NAME, node.getName());

        String[][] permTags = {
            { VdbLexicon.DataRole.Permission.ALLOW_ALTER, VdbLexicon.ManifestIds.ALLOW_ALTER },
            { VdbLexicon.DataRole.Permission.ALLOW_CREATE, VdbLexicon.ManifestIds.ALLOW_CREATE },
            { VdbLexicon.DataRole.Permission.ALLOW_DELETE, VdbLexicon.ManifestIds.ALLOW_DELETE },
            { VdbLexicon.DataRole.Permission.ALLOW_EXECUTE, VdbLexicon.ManifestIds.ALLOW_EXECUTE },
            { VdbLexicon.DataRole.Permission.ALLOW_READ, VdbLexicon.ManifestIds.ALLOW_READ },
            { VdbLexicon.DataRole.Permission.ALLOW_UPDATE, VdbLexicon.ManifestIds.ALLOW_UPDATE },
            { VdbLexicon.DataRole.Permission.ALLOW_LANGUAGE, VdbLexicon.ManifestIds.ALLOW_LANGUAGE }
        };

        for (int i = 0; i < permTags.length; ++i) {
            Property permProp = property(node, permTags[i][0]);
            Boolean value = permProp == null ? false : permProp.getBoolean();
            writeElementWithText(permTags[i][1], value.toString());
        }

        // Conditions
        visitChild(node, NodeTypeName.CONDITIONS.getId());

        // Masks
        visitChild(node, NodeTypeName.MASKS.getId());

        // End Permission
        writeEndElement();
    }

    private void dataRole(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.DATA_ROLE))
            return;

        // Data Role element
        writeStartElement(NodeTypeName.DATA_ROLE.getTag());

        // Process data role attributes
        String nameProp = node.getName();
        writeAttribute(VdbLexicon.ManifestIds.NAME, nameProp);
        Property authProp = property(node, VdbLexicon.DataRole.ANY_AUTHENTICATED);
        writeAttribute(VdbLexicon.ManifestIds.ANY_AUTHENTICATED, toString(authProp));
        Property tempTablesProp = property(node, VdbLexicon.DataRole.ALLOW_CREATE_TEMP_TABLES);
        writeAttribute(VdbLexicon.ManifestIds.ALLOW_CREATE_TEMP_TABLES, toString(tempTablesProp));
        Property grantAllProp = property(node, VdbLexicon.DataRole.GRANT_ALL);
        writeAttribute(VdbLexicon.ManifestIds.GRANT_ALL, toString(grantAllProp));

        description(node);

        // Permissions
        visitChild(node, NodeTypeName.PERMISSIONS.getId());

        // Mapped Role Names
        Property property = node.getProperty(VdbLexicon.DataRole.MAPPED_ROLE_NAMES);
        Value[] mappedRoleValues = property.getValues();
        for (Value value : mappedRoleValues) {
            writeElementWithText(VdbLexicon.ManifestIds.MAPPED_ROLE_NAME, value.getString());
        }

        writeEndElement();
    }

    private void translator(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.TRANSLATOR))
            return;

        // Translator element
        writeStartElement(NodeTypeName.TRANSLATOR.getTag());

        // Process translator attributes
        String nameProp = node.getName();
        writeAttribute(VdbLexicon.ManifestIds.NAME, nameProp);
        Property typeProp = property(node, VdbLexicon.Translator.TYPE);
        writeAttribute(VdbLexicon.ManifestIds.TYPE, toString(typeProp));
        Property descProp = property(node, VdbLexicon.Translator.DESCRIPTION);
        writeAttribute(VdbLexicon.ManifestIds.DESCRIPTION, toString(descProp));

        writeNewLine();

        // Process property attributes
        properties(node, VdbLexicon.Translator.TYPE, VdbLexicon.Translator.DESCRIPTION);

        writeEndElement();
    }

    private void source(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.SOURCE))
            return;

        // Translator element
        writeStartElement(NodeTypeName.SOURCE.getTag());

        // Process source attributes
        String nameProp = node.getName();
        writeAttribute(VdbLexicon.ManifestIds.NAME, nameProp);
        Property translatorProp = property(node, VdbLexicon.Source.TRANSLATOR);
        writeAttribute(VdbLexicon.ManifestIds.TRANSLATOR_NAME, toString(translatorProp));
        Property jndiProp = property(node, VdbLexicon.Source.JNDI_NAME);
        writeAttribute(VdbLexicon.ManifestIds.JNDI_NAME, toString(jndiProp));

        writeEndElement();
    }

    private void model(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.MODEL))
            return;

        writeStartElement(NodeTypeName.MODEL.getTag());

        writeAttribute(VdbLexicon.ManifestIds.NAME, node.getName());

        Property typeProp = property(node, CoreLexicon.JcrId.MODEL_TYPE);
        writeAttribute(VdbLexicon.ManifestIds.TYPE, toString(typeProp));

        Property pathProp = property(node, VdbLexicon.Model.PATH_IN_VDB);
        if (pathProp != null)
            writeAttribute(VdbLexicon.ManifestIds.PATH, toString(pathProp));

        Property visibleProp = property(node, VdbLexicon.Model.VISIBLE);
        if (visibleProp != null) {
            String value = toString(visibleProp);
            if (! Boolean.parseBoolean(value)) // True is the default value so no need to include if true
                writeAttribute(VdbLexicon.ManifestIds.VISIBLE, value);
        }

        description(node);

        properties(node, CoreLexicon.JcrId.MODEL_TYPE);

        // Sources
        visitChild(node, NodeTypeName.SOURCES.getId());

        // metadata element
        Property modelDefnProp = property(node, VdbLexicon.Model.MODEL_DEFINITION);
        if (modelDefnProp != null) {
            writeStartElement(VdbLexicon.ManifestIds.METADATA);
            Property metaTypeProp = property(node, VdbLexicon.Model.METADATA_TYPE);
            writeAttribute(VdbLexicon.ManifestIds.TYPE, toString(metaTypeProp));

            // TODO
            // Need to incorporate the DDL Visitor rather than using the modelDefinition property
            writeCData(toString(modelDefnProp));

            // end metadata tag
            writeEndElement();
        }
        
        // End model tag
        writeEndElement();
    }

    private void importVdb(Node node) throws XMLStreamException, RepositoryException {
        if (! isPrimaryNodeType(node, NodeTypeName.IMPORT_VDB))
            return;

        // Import-vdb element
        writeStartElement(NodeTypeName.IMPORT_VDB.getTag());

        // Process import-vdb attributes
        String nameProp = node.getName();
        writeAttribute(VdbLexicon.ManifestIds.NAME, nameProp);
        Property versionProp = property(node, VdbLexicon.ImportVdb.VERSION);
        writeAttribute(VdbLexicon.ManifestIds.VERSION, toString(versionProp));
        Property dataPoliciesProp = property(node, VdbLexicon.ImportVdb.IMPORT_DATA_POLICIES);
        if (dataPoliciesProp != null)
            writeAttribute(VdbLexicon.ManifestIds.IMPORT_DATA_POLICIES, toString(dataPoliciesProp));

        writeEndElement();
    }

    private void description(Node node) throws XMLStreamException, RepositoryException {
        Property property;
        property = property(node, NodeTypeName.DESCRIPTION.getId());
        if (property == null)
            return;

        writeElementWithText(NodeTypeName.DESCRIPTION.getTag(), toString(property));
        writeNewLine();
    }

    private void virtualDatabase(Node node) throws XMLStreamException, RepositoryException {
        // Start new document
        writeStartDocument();

        // Vdb element
        writeStartElement(NodeTypeName.VIRTUAL_DATABASE.getTag());

        // Name attribute
        Property property = property(node, VdbLexicon.Vdb.NAME);
        writeAttribute(VdbLexicon.ManifestIds.NAME, toString(property));

        // Version attribute
        property = property(node, VdbLexicon.Vdb.VERSION);
        writeAttribute(VdbLexicon.ManifestIds.VERSION, toString(property));

        writeNewLine(2);

        // Description element
        description(node);

        // Connection Type element
        property = property(node, VdbLexicon.Vdb.CONNECTION_TYPE);
        if (property != null)
            writeElementWithText(VdbLexicon.ManifestIds.CONNECTION_TYPE, toString(property));

        // Properties elements
        properties(node, VdbLexicon.Vdb.NAME, VdbLexicon.Vdb.VERSION,
                                  NodeTypeName.DESCRIPTION.getId(), VdbLexicon.Vdb.CONNECTION_TYPE);

        writeNewLine();

        //
        // Visit vdb children by name since the xsd demands them in a specific order
        //

        // Import Vdbs
        visitChild(node, NodeTypeName.IMPORT_VDBS.getId());

        // Models
        visitFilteredChildren(node, NodeTypeName.MODEL.getId());

        // Translators
        visitChild(node, NodeTypeName.TRANSLATORS.getId());

        // Data Roles
        visitChild(node, NodeTypeName.DATA_ROLES.getId());

        writeNewLine();

        // Close out the xml document
        writeEndElement();
        writeEndDocument();
    }

    @Override
    public void visit(Node node) throws RepositoryException {
        if (node == null)
            return;

        NodeType nodeType = node.getPrimaryNodeType();
        String nodeTypeName = nodeType.getName();
        NodeTypeName ntName = NodeTypeName.findName(nodeTypeName);
        try {
            switch (ntName) {
                case VIRTUAL_DATABASE:
                    virtualDatabase(node);
                    break;
                case IMPORT_VDBS:
                case TRANSLATORS:
                case SOURCES:
                case DATA_ROLES:
                case PERMISSIONS:
                case CONDITIONS:
                case MASKS:
                    visitChildren(node);
                    break;
                case MODEL:
                    model(node);
                    break;
                case IMPORT_VDB:
                    importVdb(node);
                    break;
                case TRANSLATOR:
                    translator(node);
                    break;
                case SOURCE:
                    source(node);
                    break;
                case DATA_ROLE:
                    dataRole(node);
                    break;
                case PERMISSION:
                    permission(node);
                    break;
                case CONDITION:
                    condition(node);
                    break;
                case MASK:
                    mask(node);
                    break;
                case UNKNOWN:
                default:
                    // Not a node we are interested in but may contain such nodes
                    visitChildren(node);
            }
        } catch (XMLStreamException ex) {
            throw new RepositoryException(ex);
        }
    }

    @Override
    public void visit(Property property) {
        // Not required
    }
}
